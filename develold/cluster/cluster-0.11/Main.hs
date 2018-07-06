{-# OPTIONS  -fglasgow-exts #-}
import System.IO.Unsafe
import URIHold
import ProtocolHTTP
import Cluster
import ClusterTCache
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.STM
import Data.TCache 
import System.Environment
import System.Process

data Data= Data Int deriving (Read, Show,Eq)

instance IResource Data where
   serialize x= show x
   deserialize str= read str
   --defPath _ = "data/"

   keyResource _= "int"

instance GetProto Data where
   getProto "int"= Data undefined

data Ops= Add Int deriving (Read, Show,Eq) 

instance IResource Ops where
   serialize = show 
   deserialize = read 
   keyResource _= error "not defined"
   
local= "http://127.0.0.1"


instance TransMap Ops Data where
 --mapping :: op -> ([Maybe a]-> [a])
 mapping (Add n) = sum1 where 
   sum1 [Just (Data m)] = [Data $ n+m]        `debug` "sum"
   sum1 [Nothing]= [Data $ n]
   
 commutativeOp (Add _)= True

runCommand1 str=do 
   h <- runCommand str 
   threadDelay 10000000
   return h
    
main2 = do
  
  args <- getArgs 
  case args of
    [] -> do
           let url i= local++":"++show i ++"/"
           let command0 n= "runghc Main.hs " ++ url n ++" Nothing 2>&1 | tee sal"++show n
           let command1 (n, m)= "runghc Main.hs " ++url n ++" \'Just \""++url m++"\"\' 2>&1 | tee sal"++show n
           h <- runCommand1 $ command0 80
           --hs<- mapM runCommand1 $ map command1 [(81, 80),(82,81),(83,80)]
           getChar
           --mapM terminateProcess (h:hs)
           runCommand "sudo pkill ghc"
           
    l@[url1,murl2] -> do
      print l
      main1  url1  (read murl2:: Maybe String)
    
    l -> error $ "wrong number of arguments:" ++show l

  
main1 url1 url2= do

  initCluster url1 url2 5            `debug` (show (url1,url2))
  print "" `debug` "AFTER INITCLUSTER"
  addTransaction   ["int"] ( Add 1)                `debug` "addTransaction"


  forever
  --trans <-createTrans ["int"] ( Add 10) 
  --send [trans]  $ makeURI myurl --send a request to a node of the cluster
  
  where
  forever= do
    syncCache (refcache  :: Cache (Data1 Ops Data))     `debug` "sync"
    threadDelay 10000000  `debug`"waiting"

    forever 





main= do
  id <- genIdNode                              
  let t= Tr id ["int"]  (Op ( Add 1) )
  t1 <- createTrans ["int"] ( Add 2) 
  atomically $ readTVar tvPendingTrans :: IO (Data1 Ops Data)   
  atomically $ writeTVar tvPendingTrans  $ PendingTrans []
  
  addTrans t 
  addTrans t1 -- $ MyNode node {nodesConected=["hola"]}
  addTrans t1
  addTrans t1
  node <- atomically $ readTVar tvPendingTrans :: IO (Data1 Ops Data)   
  print $ "pending="++ show node