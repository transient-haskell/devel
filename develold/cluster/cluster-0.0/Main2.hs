{-# OPTIONS  -fglasgow-exts #-}
import System.IO.Unsafe
import URIHold
import ProtocolHTTP
import Cluster
import ClusterTCache
import TCache
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.STM

data Data= Data Int deriving (Read, Show)

instance Serializable Data where
   serialize x= show x
   deserialize str= read str
   --defPath _ = "data/"
instance IResource Data where
   keyResource _= "int"

instance GetProto Data where
   getProto "int"= Data undefined

data Ops= Add Int deriving (Read, Show)

instance Serializable Ops where
   serialize op= show op
   deserialize str= read str

myurl= "http://127.0.0.1:90"
clusterurl= "http://127.0.0.1:80"
 

instance TransMap Ops Data where
 --mapping :: op -> ([Maybe a]-> [a])
 mapping (Add n) = sum1 where 
   sum1 [Just (Data m)] = [Data $ n+m]
   sum1 [Nothing]= [Data $ n]



--TransMap  Ops Data
--instance Trans (Tr (Op Ops)) (Data1 Ops Data) Id where
--instance   Cluster  (Tr (Op Ops)) (Data1 Ops Data) URIHold where

main= do
  initCluster myurl clusterurl
  print " DE VUELTA"
  addTransaction  ["int"] ( Add 1) 
  --trans <-createTrans ["int"] ( Add 10) 
  --send [trans]  myuri --send a request to a node of the cluster
  
  --threadDelay 1000000000
  --syncCache (refcache ::Cache (Data1 Ops Data))
  forever
  where
  forever=forever



