{-# OPTIONS -fglasgow-exts #-}


-------------------------------------------------
-- A Transactional cache with configurable persitence
-- (Something like a little Java Hybernate or Rails for Rubi)
-- Author: Alberto Gómez Corona Nov 2006
-- Language: Haskell
-- Version: 0.4
-- Terms of use: you can do whatever you want
-- with this code as long as you keep this notice

-- 10/15/2007 : changes
-- Default writeResource and delResource for persistence in files
--     (only keyResource must be defined by the user if use defaults)
-- Coherent Inserts and deletes
-- Reduced the number of accesses to the hashtable
-- hashtable access put outside of the transaction block (takeBlocks) 
--    faster re-executions in case of roll-back

------------------------------------------------

 
module TCache (

IResource(..)              -- class interface to be implemented for the object by the user

,Operation (Insert,Delete) -- data definition used to communicate object Inserts and Deletes to the cache

,Cache            -- :: IORef (Ht a,Int, Integer)     --The cache definition 

,withResourcesID  -- :: (IResource a)=> [a]->         --list of resources to be extracted for the user function
                 --    ([Maybe a]->[Operation a])    --user function that get the retrieved resources
		 --    ->IO ()                       --and return a list of  objects to be inserted/modified or deleted

,withResources   -- :: (IResource a)=> [a]            --list of resources to be retrieve
                 --   ->([Maybe a]->[a])             ----function that get the retrieved resources
                 --   ->IO ()                        --and return a list of  objects to be inserted/modified 

,withResource    -- :: (IResource a)=> a              --same as withResources , but for one only object
                 --   ->([Maybe a]->a)               --
                 --   ->IO ()                        --

,getResources    -- :: (IResource a)=>[a]             --resources [a] are read from cache and returned
                 --   -> IO [Maybe a]   

,getResource     -- :: :: (IResource a)=>a            --to retrieve one object instead of a list
                 --   -> IO [Maybe a]   

,deleteResources -- :: (IResource a)=>[a]-> IO()      -- delete the list of resources from cache and from persistent storage
,deleteResource  -- :: (IResource a)=>a-> IO()        -- delete the  resource from cache and from persistent storage
--cache handling

,refcache        -- :: Cache a                        --the reference to the cache (see data definition below)

,syncCache       -- :: (IResource a) =>Cache a -> IO() --force the atomic write of all the cache objects into permanent storage
                                                       --useful for termination

--start the thread that clean and writes on the persistent storage trough syncCache
,clearSyncCacheProc  -- :: (IResource a) =>Cache a       --The cache reference                        
                     --   -> Int                         --number of seconds betwen checks
                     --   -> (Integer->Integer-> Bool)   --The user-defined check-for-cleanup-from-cache for each object 
                                                         --(when True, the object is removed from cache)
                     --   -> Int                         --The max number of objects in the cache, if more, the cleanup start
                     --   -> >IO ThreadId                --Identifier of the thread created

-- the default check procedure
,defaultCheck    -- :: Integer                          --last access time for a given object
                 --   ->Integer                        --last cache syncronization (with the persisten storage)
                 --   ->Bool                           --return true for all the elems not accesed since  
                                                       --half the time between now and the last sync
                                                     
-- auxiliary
,readFileStrict  -- :: String -> IO String            -- Strict file read, needed for persistence trough files                                                                 


)
where

import GHC.Conc
import Control.Exception as Exception
import Control.Concurrent
import Data.HashTable as HTable
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.Time 
import Data.Maybe(catMaybes,mapMaybe)
import Debug.Trace
import System.Directory


debug1 a b= trace b a


type Key= String

-- Interface that must be defined for every object being cached
-- readResource and writeResource implemented by default as read/write files with its key as filename
-- Read and Show are specified just to allow these defaults
class (Read a, Show a)=>IResource a where
	keyResource  :: a->String        -- return unique key

	readResource :: a->IO (Maybe a)  -- get object content from persistent media (NOTE: must be strict, not lazy )

        readResource x= do 
             s <- readFileStrict $ keyResource x 
             return $ read s        

	writeResource:: a->IO()          -- write to persistent media
        writeResource x= writeFile (keyResource x) (show x) 

	delResource:: a->IO()            -- delete in persistent media
	delResource x= removeFile $ keyResource x


-- to allow not only inserts but also deletes
data Operation b= Insert b | Delete b

	
type AccessTime= Integer
type ModifTime = Integer

type Block a=  (TVar a,AccessTime,ModifTime)
type Ht a= HashTable String (Block a)
-- contains the hastable, number of items, last sync time
type Cache a= IORef (Ht a,Int, Integer)

-- the cache holder

refcache :: Cache a 
refcache =unsafePerformIO $  do c <-  HTable.new (==) hashString
				newIORef (c,0,nowTime 1)


	
withResourcesID:: (IResource a)=> [a]->([Maybe a]->[Operation a])->IO ()
withResourcesID rs f=  do
	(cache,_,_) <- readIORef refcache
        mtrs  <- takeBlocks rs cache False
	idrs <- atomically $ do
	    mrs <- mapM mreadTVar mtrs
	    let idrs= f mrs
            let ladd= map selectAdd idrs
            releaseBlocks (catMaybes ladd) cache
            return idrs

        let ldel= map selectDelete idrs
        let ldel1= catMaybes ldel
	mapM delResource ldel1
	delListFromHash cache  $ map keyResource ldel1
	    
	return ()

        where

	selectDelete (Insert a)= Nothing
	selectDelete (Delete a)= Just a
	
	selectAdd (Insert a)= Just a
	selectAdd (Delete a)= Nothing

mreadTVar (Just tvar)= do r <-readTVar tvar
                          return $ Just r
mreadTVar Nothing    =    return Nothing
 



withResource:: (IResource  a)=> a->(Maybe a->a)->IO ()
withResource r f= withResources [r] (\[mr]-> [f mr])




withResources:: (IResource a)=> [a]->([Maybe a]->[a])->IO ()
withResources rs f=  do
	(cache,size,time) <- readIORef refcache 
        mtrs  <-   takeBlocks rs cache False 
         
        
	atomically $ do 
	    
            mrs <- mapM mreadTVar mtrs
	    let rs'=  f mrs
	    releaseBlocks rs' cache  
	
	return ()
        
takeBlocks :: (IResource a)=> [a] -> Ht a -> Bool ->IO [Maybe (TVar a)] 
takeBlocks rs cache addToHash=do rs' <- mapM checkBlock  rs   
                                 return $  rs' `using` seqList rwhnf
  where		
    
    -- must be sure to execute the "atomic newTVar" statements  before entering any further atomic block
    -- Taken from Control.Parallel.Strategies 
    rwhnf x = x `seq` ()
    using x s = s x `seq` x

    seqList strat []     = ()
    seqList strat (x:xs) = strat x `seq` (seqList strat xs)
    ---------
    
    checkBlock  r =do
	c <-  HTable.lookup cache keyr  `debug1` ("checkBlock: "++ show (keyResource r))
	case c of
		Nothing   -> do
                  mr <- readResource r `debug1` ("read "++keyr++ " hash= "++ (show $ hashString  keyr))
		  case mr of
                    Nothing -> return Nothing  
	            Just r2 -> do 
                        tvr <- atomically $! newTVar r2  
			case addToHash of
                           False -> return $ Just tvr
                           True  -> do 
			            update cache keyr (tvr, ti, 0) -- accesed, not modified
                                    return $ Just tvr
                           

		Just(tvr,_,_)  -> return $ Just tvr `debug1` "in cache"

	where 	keyr= keyResource r
		ti= t where TOD t _=unsafePerformIO getClockTime


releaseBlocks rs cache =do
    mapM_ checkBlock  rs                   
    
 where
    checkBlock  r =do
	c <- unsafeIOToSTM $ HTable.lookup cache keyr
	case c of
	    Nothing   -> do tvr <- newTVar r  
			    unsafeIOToSTM $ update cache keyr (tvr, ti, ti ) -- accesed and modified XXX
								 
				
	    Just(tvr,_,tm)  ->do  writeTVar tvr r
				  unsafeIOToSTM $ update cache keyr (tvr ,ti,ti)
									
					
						
	where 	keyr= keyResource r
		ti= t where TOD t _=unsafePerformIO getClockTime



getResource r= do{mr<-getResources [r];return $! head mr}

getResources:: (IResource a)=>[a]-> IO [Maybe a]
getResources rs= do
   (cache,_,_) <- readIORef refcache 
   mtrs <- takeBlocks rs cache True
   atomically $ mapM mreadTVar mtrs
		

deleteResource r= deleteResources [r]
deleteResources rs=do   
   (cache,_,time) <- readIORef refcache 
   atomically $! do
	unsafeIOToSTM $ mapM delResource rs
	unsafeIOToSTM $ delListFromHash cache $ map keyResource rs
        
 where
    ntime= nowTime 1

delListFromHash  hash l= do{mapM (delete hash) l; return()}

updateListToHash hash kv= do{mapM (update1 hash) kv; return()}where
	update1 h (k,v)= update h k v

-----------------------clear, sync cache-------------
clearSyncCacheProc ::(IResource a)=> Cache a->Int->(Integer->Integer->Bool)->Int->IO ThreadId
clearSyncCacheProc refcache time check sizeObjects= 
  	forkIO $ clear refcache time check sizeObjects

 where clear refcache time check sizeObjects= do
    	threadDelay $ (fromIntegral$ time * 1000000)
    	clearSyncCache refcache time check sizeObjects
    	clear refcache  time check sizeObjects
 

syncCache refcache = do
   (cache,_,_) <- readIORef refcache 
   list <- toList cache
   atomically $ save list 0 
   print $ "write to persistent storage finised: "++ show (length list)++ " objects" 

--  - saves the unsaved elems of the cache
--  - delete some elems of  the cache when the number of elems > sizeObjects
--  - The deletion depends on the check criteria. defaultCheck is the one implemented
clearSyncCache ::(IResource a) => Cache a-> Int -> (Integer->Integer-> Bool)-> Int -> IO ()
clearSyncCache refcache time check sizeObjects=do
   (cache,size,last) <- readIORef refcache 
   handle (\e->do{print e;return ()})$ do
    (nsize,ntime) <- atomically $  clearCache cache size last check sizeObjects
    writeIORef refcache (cache,size, ntime) 

save:: (IResource a) => [(String, Block a)]-> Integer-> STM ()
save list lastSave= mapM_ save1 list 
  where
	save1(_, (tvr,_,modTime))= do  
		if modTime > lastSave 
		  then do
			r<-  readTVar tvr
			unsafeIOToSTM $! writeResource r  `debug1` ("saved " ++ keyResource r)
		  else return()

nowTime x = t where TOD t _=unsafePerformIO getClockTime



clearCache:: (IResource a) =>Ht a->Int->Integer->
              (Integer->Integer-> Bool)->Int -> STM (Int,Integer)

clearCache cache size lastSync check sizeObjects= do
	elems <- unsafeIOToSTM $ toList cache
	let size=length elems
	save elems lastSync 
	if size > sizeObjects then filtercache lastSync elems else return (size,lastSync) 


  where
        -- delete elems from the cache according with the check criteria
	filtercache lastSync elems= do
	    n <- unsafeIOToSTM $ mapM filter elems 
	    return(size - sum n,nowTime 1)
	    where
		check1 (_,lastAccess,_)=check lastAccess lastSync 

		filter ::(String,Block a)->IO Int
		filter (k,e)=  if check1 e then do{HTable.delete cache k;return 1} else return 0

--to drop from the cache all the elems not accesed since  half the time between now and the last sync
defaultCheck:: Integer->Integer->Bool
defaultCheck lastAccess lastSync
	| lastAccess > halftime = False
	| otherwise  = True

        where 
        halftime= now- (now-lastSync) `div` 2
        now= nowTime 1
  


readFileStrict f = do
  h <- openFile f ReadMode
  s <- hFileSize h
  let n= fromIntegral s
  str <- readn h n
  return str

  where
  -- read n bytes from handle h
  readn h 0= return ""
  readn h n=do	
        str <- hGetContents h
        -- force to read n elements
        if (str !! (n-1))== '\x00' then return str else return str


