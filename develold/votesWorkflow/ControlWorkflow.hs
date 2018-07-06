{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances  -fallow-undecidable-instances -O2  #-}
{-transparent low level support (state logging, resume of the computation state, wait for data condition) for very long time 
  computations. Workflow give the two first services to any monadic computation of type  (a-> m a)  
  
                    f x >>=\x'-> g x' >>= \x''->... z by 
                    
                    prefixing the user with the method step: 
                    
                         step f  x >>= \x'-> step g  x' >>= \x''->...  
                   
in this way, a workflow can be described with the familiar "do" notation. In principle, there is no other limitation
on the syntax but the restriction (a -> m a): All computations consume and produce the same type of data.
                             
Alberto Gomez Corona agocorona@gmail.com 2008
-}
module ControlWorkflow (
    Workflow --    a useful type name
    ,WorkflowStep
    ,WorkflowList 
    ,Stat 

    ,step -- :: (Monad m) =>  (a -> m a) -> ( a -> Workflow m a) 
          -- encapsulates a monadic computation into state monad  that brings persistence and
          -- recovery services

    ,startWF -- :: (Monad m) => 
             --     String ->           mame of workflow in the workflowlist
             --     a ->                initial data value 
             --     WorkflowList m a -> assoc-list of (workflow name string,Workflow methods)
             --     m a                 resulting value
             -- start or continue a workflow
             

    , restartWorkflows -- :: (IResource  a, Serialize a) =>  WorkflowList IO a -> IO ()
                       -- re-start the non finished workflows. needs the assoclist. 
   

    , getStep -- Monad m => Int -> Workflow m a  return the n-tn intermediate result
              --                                 if Int < 0 count from the current result back

    , getAll  -- :: Monad m => Workfow m  [a]  return all the intermediate results

    , unsafeIOtoWF -- executes a IO operation. this is executed  whenever re-started, no matter where is the resume point
                   -- This is useful for external IO re-initializations not controllable by the State monad.


    , waitFor -- ::(IResource a, Serialize a) => (a ->Bool) -> a -> IO a
              -- wait until a object (with a certaing key=keyResource x) meet a certain condition 
              -- (useful for checking external actions, possibly by other workflows or by direct use of TCache primitives ) 

    ,syncWrite -- syncWrite:: Monad m => 
               -- Bool ->  True means that changes are inmediately saved after each step
               -- Int ->  number of seconds between saves when async
               -- Int ->  max size of the cache when async
               -- WF m (Stat a) ()   in the workflow monad
               
               -- Turn on and off syncronized writing to disk
               -- select async mode only 
               --       -for very fast workflow steps  or 
               --       -when the cache policies are dictated outside of the workflow 
               --        trough SyncCacheProc (see TCache module)
               
)


where


import System.IO.Unsafe
import Control.Monad(when,liftM)
import Unsafe.Coerce
import Control.Concurrent (forkIO)
import Control.Concurrent.STM(atomically, retry, readTVar)
import Debug.Trace

import Data.TCache.Dynamic
import Data.RefSerialize
import Data.List((\\),find,elemIndices)
import Data.Typeable

debug a b = trace b a

data WF m s l = WF { st :: s -> m (s,l) }

type Workflow m l= WF m (Stat l) l  -- not so scary

type WorkflowStep m a= ( a -> Workflow m  a)

type WorkflowList m a = [(String,  WorkflowStep m a)]

data Stat a=  Workflows [(String,String)]
           |Stat{ wfName :: String, state:: Int, index :: Int, recover:: Bool, sync :: Bool , versions :: [a]} 
         
           deriving (Typeable)

stat0 = Stat{ wfName="", state=0, index=0, recover=False, versions =[], sync= True}


-- serialization of data is done trough RefSerialize because it permits to store
-- different versions of the same object with minumum memory.

instance Serialize a =>  Serialize (Stat a) where
    showp (Workflows list)= do
      str <- showp list
      return $ "StatWorkflows "++ str
              

    showp  (Stat wfName state index recover sync versions )= do
       parsea <- rshowp  versions 
       return $ "Stat "++ show wfName ++" "++ show state++" "++show index++" "++show recover++" "++ show sync ++ parsea  
       
    readp = choice [rStat, rWorkflows] where
        rStat= do
              symbol "Stat" 
              wfName  <- stringLiteral
              state   <- integer
              index   <- integer
              recover <- bool
              sync    <- bool
              versions<- rreadp
              return $ Stat wfName (fromIntegral state) (fromIntegral index) recover sync versions 
              


        rWorkflows= do
               symbol "StatWorkflows"
               list <- readp
               return $ Workflows list
        
--persistence trough TCache   , default persistence in files      
        
instance (IResource a, Serialize a,Typeable a)=> IResource (Stat a) where
   keyResource Stat{wfName=name, versions = []}= prefix ++name
   keyResource Stat{wfName=name, versions = (a:_)}= prefix ++name++"."++keyResource a

   keyResource w@(Workflows xs)= "StatWorkflows"

 
   defPath x= "Workflows/" ++ show (typeOf x)++"/"  -- directory for Workflow data

   serialize x= runW $ showp x
   deserialize str = runR readp str

prefix= "Stat."
lengthPrefix= length prefix

insertDResources xs=  withDResources [] (\_-> xs)

--unsafeIOtoWF ::  Monad m => IO a -> WF m (Stat b) a
unsafeIOtoWF x= let y= unsafePerformIO x in y `seq` return y




instance Monad m =>  Monad (WF m s) where
    return  x = WF (\s ->  return  (s, x)) 
    WF g >>= f = WF (\s ->do
                (s1, x) <- g s 
                
                let WF fun=  f x 
                (s3, x') <- fun s1 
                
                return (s3, x'))
  
class (IResource  a, Serialize a,Typeable a) => Workflow_ a where
  -- | step lift a monadic computation (a -> m a) in in to the WF monad, provides state loging and automatic resume
  step :: (Monad m) =>  (a -> m a) -> ( a -> Workflow m a)
  step f =  \x -> WF(\s -> do 
        let stat= state s
        let ind= index s
        if recover s && ind < stat
          then  return (s{index=ind +1 },   versions s !! (stat - ind-1) )
          else do
                x'<- f x
                let s'= s{recover= False, versions =  x': versions s, state= state s+1} 
                unsafeIOtoWF $ do

                let 
                     doit1 xs
                          | keyResource s /= keyResource s' -- `debug` ("keys:"++keyResource s ++", "++keyResource s')
                               =  
                                  let newlist= newpair :(xs \\[oldpair])
                                      newpair= (keyResource s',original)
                                      oldpair= (key,original)
                                      key= keyResource s
                                      original= case lookup key xs of
                                                    Nothing   -> error $ "workflow stat not found: " ++key
                                                    Just old  -> old
                                                    
                                  in [Insert $ toIDyn $ (Workflows newlist :: Stat a)
                                     ,Delete $ toIDyn s, Insert  $ toIDyn s'
                                     ,Insert (toIDyn x')] 
                                          -- `debug`("insert "++keyResource s'++","++keyResource x'++" delete: "++
                                          --         keyResource s++","++keyResource x)
                          | otherwise= [Insert $ toIDyn s', Insert $ toIDyn x'] 
                                          -- `debug`("insert "++keyResource s'++","++keyResource x')
                let
                  doit [Nothing] =   doit1  []
                  doit [Just d] = let Workflows xs= (fromIDyn d :: Stat a) 
                                  in doit1 xs 

                withDResourcesID [toIDyn $ (Workflows undefined :: Stat a)] doit
                
                when (sync s) $ unsafeIOtoWF $ syncCache 
                                
                return (s', x') )
                
  -- | start or continue a workflow. WorkflowList is a assoclist of (name, workflow computation)    
  startWF :: (Monad m) => String -> a -> WorkflowList m a ->m a 
  startWF name v wfs= do
          unsafeIOtoWF $ print "XXXX unsafeIOtoWF XXXXXXX" `debug` "debug unsafe"
          unsafeIOtoWF $ registerType $ (Workflows [] :: Stat a) `debug`("XXXXX"++ (show $ typeOf (Workflows [] :: Stat a)))
          case lookup name wfs of  
           Nothing -> error $ "MonadWF.startWF: workflow not found: "++name; 
           Just f -> do

             let stat1= stat0{index=0,wfName= name,versions=[v]} ::Stat a
             let key= keyResource stat1 
             (vn, stat, found) <- do 
                   wxs <- unsafeIOtoWF $ getResource $ (Workflows undefined :: Stat a)
                   case wxs of
                    Nothing -> return (v,stat1, False)
                    Just (Workflows xs) -> 
                     case find (\(_,s)-> s == key) xs of  
                               
                        Just (key1, oldkey1) -> do 
                                -- already in course
                                mst <- unsafeIOtoWF $ getResource stat0{wfName= drop lengthPrefix key1}
                                case mst of
                                  Nothing -> error $ "no stat for key. " ++ key
                                  Just s@Stat{versions=(a:_)} -> return (a,s{index=0,recover=True}, True)  -- the last value
                        Nothing -> return (v, stat1, False)
             -- insert it in the running workflow list
             when (found == False) $ 
               let addWF [Nothing] = [Workflows [(key,key)],stat]
                   addWF [Just (Workflows xs)]=  [Workflows ((key,key):xs),stat]
                             
               in unsafeIOtoWF $ withResources [(Workflows undefined :: Stat a)] addWF 
                                     
             runWF name f vn stat  -- `debug` (serialize stat)
                



  restartWorkflows :: (IResource  a, Serialize a) =>  WorkflowList IO a -> IO ()
  restartWorkflows map = do
          unsafeIOtoWF $ registerType $ (Workflows [] :: Stat a)
          mw <- getResource ((Workflows undefined ) :: Stat a) 
          case mw of
            Nothing -> return ()
            Just (Workflows all) -> mapM_ start all
          where
            start :: (String, String) -> IO ()
            start (key,_)= do
              let name= let [init,end]= elemIndices '.' key 
                            
                            start= drop (init + 1) key
                        in take (end-init -1) start    
              case  lookup name map of
                 Just f -> do
                             Just st <- getResource stat0{ wfName=key}
                             
                             forkIO $ runWF key f (head $ versions st) st >> return ()
                             return ()
                 Nothing -> error $ "workflow not found: "++ name

 
  
  runWF :: (Monad m,IResource  a, Serialize a) => String ->( a -> Workflow m a) ->  a -> (Stat a) -> m  a
  runWF name f v s=do
 
           (s', v')  <-  st (f v) $ s   
           
           let  key= keyResource s'
           
           let  delWF [Nothing] = error $ " Workflow list not found: " 
                delWF [Just d]= let Workflows xs= (fromIDyn d :: Stat a) in
                    case lookup key xs of  
                        Nothing -> error $"runWF not found state for key: "++ key      
                        Just oldkey ->
                          (map (Delete . toIDyn) $ versions s')  ++    -- delete all intermediate objects generated                                                              
                          [Insert . toIDyn  $ (Workflows (xs \\ [(key,oldkey)]) ::Stat a)
                          ,Delete $ toIDyn s'] 

                                      
           unsafeIOtoWF $ withDResourcesID  [toIDyn $ (Workflows undefined :: Stat a)]  delWF 
           when (sync s) $ unsafeIOtoWF $ syncCache 
           return v'

  -- switch on and off syncronous write for each step (default is syncronous)
  -- for very fast steps, asyncronous is better.h
  -- when TCache is used for other purposes, is better to define the cache policy direct
  syncWrite:: (Monad m , IResource a)=> Bool -> Int  ->  Int ->  WF m (Stat a) ()
  syncWrite bool time maxsize= WF(\s ->do
                when (bool== False) $ do
                    unsafeIOtoWF $ clearSyncCacheProc  time defaultCheck maxsize
                    return ()
                return (s{ sync= bool},()))


instance (IResource  a,Serialize a,Typeable a) => Workflow_ a



--  return the result of the previous step
getStep :: Monad m => Int -> Workflow m a
getStep i=  WF(\s ->do 
                let  stat= state s 
                return (s, if i > 0 && i <= stat then versions s !! (stat - i) 
                           else if i < 0 && i >= -stat then versions s !! (stat +i) 
                           else error "getStep: wrong index")
                )
                
-- get all the step results
getAll :: Monad m => WF m (Stat a) [a]
getAll =  WF(\s ->return (s, take (state s) $ versions s))

{-

exec :: (Monad m) =>  WF m s a -> s -> m a
exec (WF f) s = do (s', x) <- f s
                   return x 

run :: (Monad m,Monad (WF m s)) =>  (a -> WF m s a) -> s -> a -> m a
run f s a= exec (f a) s

-}

     
--------- event handling--------------     
reference x=do
             mv <- getDTVars [x ]
             case mv of
               [Nothing] -> do 
                       insertDResources [x]
                       reference x
             
               [Just cl] -> return cl

type Filter a= (a -> Bool)
-- wait until a object (with a certaing key=keyResource x) meet a certain condition (useful to check external actions ) 
-- a --a -> WF m (Stat a)  a
--waitFor :: (IResource a, RefSerialize a) =>  Filter a -> a -> WF IO (Stat a) a
--waitFor filter x= (step $  waitFor1 filter) x where
--NOTE if you Delete the object from te cache, waitFor will no longuer work
waitFor ::  (IResource a, Serialize a, Typeable a) => Filter a -> a -> IO a
waitFor  filter x=  do
        tv <- reference (toIDyn $  x) 
        atomically $ do      
                dyn  <- readTVar tv
                let  x= fromIDyn dyn
                case filter x   
                 of 
                        False -> retry
                        True  -> return x 
                          


