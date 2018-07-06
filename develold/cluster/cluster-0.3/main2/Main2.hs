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
   serialize op= show op
   deserialize str= read str

myurl= "http://127.0.0.1:90/"
clusterurl= "http://127.0.0.1:80/"
 

instance TransMap Ops Data where
 --mapping :: op -> ([Maybe a]-> [a])
 mapping (Add n) = sum1 where 
   sum1 [Just (Data m)] = [Data $ n+m]               `debug` "sum1"
   sum1 [Nothing]= [Data $ n]                        `debug` "sum1"



--TransMap  Ops Data
--instance Trans (Tr (Op Ops)) (Data1 Ops Data) Id where
--instance   Cluster  (Tr (Op Ops)) (Data1 Ops Data) URIHold where

main= do
  initCluster myurl clusterurl

  addTransaction  ["int"] ( Add 1)  `debug` "DE VUELTA"
  --trans <-createTrans ["int"] ( Add 10) 
  --send [trans]  myuri --send a request to a node of the cluster
  
  forever
  where
  forever= do
    syncCache (refcache ::Cache (Data1 Ops Data))
    threadDelay 1000000

    forever



