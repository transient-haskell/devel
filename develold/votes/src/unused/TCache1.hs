{-# OPTIONS -fglasgow-exts #-}
module TCache where

-------------------------------------------------
-- A Transactional cache with configurable persitence
-- (Something like the Java Hibernate)
-- Author: Alberto Gómez Corona Nov 2006
-- Language: Haskell
-- Version: 0.2
-- Terms of use: you can do whatever you want
-- with this code as long as you keep this notice

--10/15/2007 :
--Coherent Inserts and deletes
--Reduced the number of accesses to the hashtable
------------------------------------------------

 
import GHC.Conc
import Control.Exception as Exception
import Control.Concurrent
import Data.HashTable as HTable
import Data.IORef

import System.IO.Unsafe
import System.Time 
import Data.Maybe(catMaybes,mapMaybe)
import Debug.Trace

debug1 a b= trace b a


type Key= String

class IResource a where
	keyResource  :: a->String
	readResource :: a->IO (Maybe a)
	writeResource:: a->IO()
	delResource:: a->IO()
	
-- to allow not only inserts but also deletes
data Res b= Insert b | Delete b

	
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


	
withResourcesID:: (IResource a)=> [a]->([Maybe a]->[Res a])->IO ()
withResourcesID rs f=  do
	(cache,_,_) <- readIORef refcache
	atomically $ do
	    mrs  <- takeBlocks rs cache False
	    let idrs= f mrs
	    let ldel= map selectDelete idrs
            let ladd= map selectAdd idrs
            let ldel1= catMaybes ldel

	    unsafeIOToSTM $ mapM delResource ldel1
	    unsafeIOToSTM $ delListFromHash cache  $ map keyResource ldel1
	    
	    releaseBlocks (catMaybes ladd) cache  
	return ()
  where

	selectDelete (Insert a)= Nothing
	selectDelete (Delete a)= Just a
	
	selectAdd (Insert a)=Just a
	selectAdd (Delete a)=Nothing
	




withResource:: (IResource a)=> a->(Maybe a->a)->IO ()
withResource r f= withResources [r] (\[mr]-> [f mr])

withResources:: (IResource a)=> [a]->([Maybe a]->[a])->IO ()
withResources rs f=  do
	(cache,size,time) <- readIORef refcache
	atomically $ do
	    mrs  <- takeBlocks rs cache False
	    let rs'=  f mrs
	    releaseBlocks rs' cache  
	
	return ()
	
	
takeBlocks :: (IResource a)=> [a] -> Ht a -> Bool ->STM [Maybe a]
takeBlocks rs cache addToHash=  mapM checkBlock  rs  

  where		
    -- checkBlock :: (IResource a)=> a-> STM(Maybe a)
    checkBlock  r =do
	c <-  unsafeIOToSTM $ HTable.lookup cache keyr
	case c of
		Nothing   -> do
                        r' <- unsafeIOToSTM  $ readResource r `debug1` ("read "++keyr++ " hash= "++ (show $ hashString  keyr))
			case (addToHash, r') of
                           (False,_)-> return r'
                           (True,Just r2) -> do tvr <- newTVar r2  
			                        unsafeIOToSTM $ update cache keyr (tvr, ti, 0) -- accesed, not modified
                                                return  r'
                           _ -> return r'

		Just(tvr,_,_)  ->do	r' <- readTVar tvr 
					return $ Just r'

	where 	keyr= keyResource r
		ti= t where TOD t _=unsafePerformIO getClockTime

releaseBlocks rs cache =do
    mapM_ checkBlock  rs
    --unsafeIOToSTM $ updateListToHash cache  bls
    
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
   atomically $! do 
		mrs<- takeBlocks rs cache True
		-- releaseBlocks (catMaybes mrs) cache 
		return mrs

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
   --print $ "Cache syncronization: "++ show (length list)++ " objects" 
   atomically $ save list 0 

--initializes the process that
--  - saves the unsaved elems of the cache
--  - delete some elems of  the cache when the number of elems > sizeObjects
--  - The deletion depends on the check criteria. defaultCheck is the one implemented

clearSyncCache refcache time check sizeObjects=do
   (cache,size,last) <- readIORef refcache 
   handle (\e->do{print e;return ()})$
    atomically $ do
	
	(nsize,ntime) <-  clearCache cache size last check sizeObjects
	unsafeIOToSTM $ writeIORef refcache (cache,size, ntime) 



save:: (IResource a) => [(String, Block a)]-> Integer-> STM ()
save list lastSave= mapM_ save1 list 	
  where
	save1(_, (tvr,_,modTime))= do  
		if modTime > lastSave 
		  then do
			r<-  readTVar tvr
			unsafeIOToSTM $! writeResource r
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
		check1 (_,t,_)=check t lastSync 

		filter ::(String,Block a)->IO Int
		filter (k,e)=  if check1 e then do{HTable.delete cache k;return 1} else return 0

--select the elems to be deleted that last accesed before half the time interval
--between now and the last clear time
defaultCheck lastSave t
	| t< mintime = True
	| otherwise  = False
		where 	now= nowTime 1 
			mintime= now- (now-lastSave) `div` 2
