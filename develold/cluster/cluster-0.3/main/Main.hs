{-# OPTIONS  -fglasgow-exts #-}
import System.IO.Unsafe
import URIHold
import ProtocolHTTP
import Cluster
import ClusterTCache
import Data.TCache
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.STM

data Data= Data Int deriving (Read, Show)

instance IResource Data where
   serialize x= show x
   deserialize str= read str
   --defPath _ = "data/"

   keyResource _= "int"

instance GetProto Data where
   getProto "int"= Data undefined

data Ops= Add Int deriving (Read, Show) 

instance IResource Ops where
   serialize = show 
   deserialize = read 
   keyResource _= error "not defined"
   
myurl= "http://127.0.0.1:80/"


instance TransMap Ops Data where
 --mapping :: op -> ([Maybe a]-> [a])
 mapping (Add n) = sum1 where 
   sum1 [Just (Data m)] = [Data $ n+m]
   sum1 [Nothing]= [Data $ n]
   
 commutativeOp (Add _)= True

main= do
  initCluster myurl myurl
  print "" `debug` "AFTER INITCLUSTER"
  addTransaction  ["int"] ( Add 1)  
  --trans <-createTrans ["int"] ( Add 10) 
  --send [trans]  $ makeURI myurl --send a request to a node of the cluster
  
  threadDelay 10000000000
  where
  forever= do
    syncCache (refcache ::Cache (Data1 Ops Data))
    

    forever 





