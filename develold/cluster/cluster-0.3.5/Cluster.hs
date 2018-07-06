{-# OPTIONS  -fglasgow-exts -XUndecidableInstances   #-}
module Cluster where

import Control.Concurrent
import System.IO.Unsafe
import Data.List
import System.Time
import Transaction
import Debug.Trace
import Control.Monad
import Control.Concurrent.STM(STM,TVar)
--import Control.Concurrent.STM.TMVar(TMVar)
import Data.Typeable
import URIHold
import Data.TCache.Dynamic
import Protocol
import ProtocolHTTP

type TMVar= TVar

debug :: x -> String -> x
debug a b= trace (f b) a where
 f str= let prefix = unsafePerformIO $ do
                                                     th <- myThreadId
                                                     time <- getTime
                                                     return $ show time ++ " "++ show th
        in prefix ++": "++str

debug1 x str =     debug x (str++" return= "++ show x)

class Traceable x where
    debugf ::  x -> String -> x


instance Show x => Traceable (IO x)  where
   debugf iox str = do
              x <- iox
              return $ debug x (str++" return= "++ show x)

instance Show x => Traceable (STM x) where
  debugf stmx str=   do
              x <- stmx
              return $  debug x (str++" return= "++ show x)

{-
instance Show x => Traceable x where
   debugf x str =     debug x (str++" return= "++ show x)
-}




data CClockTime= TTOD Integer Integer deriving (Read, Show, Eq, Ord)

-- transactions where the id of the transactions include the  node number in the cluster
type NodeName=  String

data Seq = Seq Int deriving (Eq,Ord, Read, Show)

data Id= Id {time:: CClockTime, seq :: Seq, node:: NodeName}  deriving (Eq,Read,Show)


id0= Id (TTOD 0 0) (Seq 0) ""

genId= do liftM3 Id getTime genSeq  (return  "")

getTime=   do
            TOD t1 t2 <- getClockTime
            return $ TTOD t1 t2

seqVar:: MVar Int
seqVar= unsafePerformIO $ newMVar 0

genSeq= liftM Seq  $ modifyMVar seqVar (\x -> let y=x+1 in return (y,y))




instance Ord Id where
  compare (Id t s n) (Id t' s' n')=
           case compare t t' of
              LT -> LT
              GT -> GT
              EQ -> case compare n n' of
                    LT -> LT
                    GT -> GT
                    EQ ->  compare s s'




myNodeName = unsafePerformIO $ newMVar  ""

genIdNode :: IO Id
genIdNode = do
      id     <- genId
      mynode <- readMVar myNodeName
      return   id{node= mynode}



class Trans t  a id  => SyncRepository t a id | t-> a, t-> id  where
   reify :: t -> STM t
   applyTrans :: [t] -> STM [t]
   addTrans :: t -> IO ()
   emptyTrans :: STM t
   --tvAllObjects :: TVar a
   --tvPendingTrans :: TVar a
   --tvMyNode :: TVar a


class Trans t a Id => NTrans t a  where
    getNode :: t -> String




data Nod= Nod
             { uri:: URIHold
             , number :: Int
             , nodesConected::[NodeName]
             , lastReceived:: Id
             , consolidatedId:: Id
             , active:: State
             , prefix :: String
             ,isFinalNode :: Bool } deriving(Read,Show)

instance Eq Nod where
  n1 == n2 = uri n1==uri n2

nod0= Nod (makeURI "http://localhost")   0  [] id0 id0 Inactive "" False

data State= Inactive
          | ConnectReceived Int Id
          | ConnectSent
          | ToSendAddObjects Id
          | AddObjectsSent
          | ToSendConnect
          | Active
          deriving(Read,Show,Eq)

data ClusterData= MyNode Nod
                | Node Nod
                | AllObjects Int [(String,Id)]
                deriving (Read,Show, Typeable)



class (Protocol (nodeId,Id,Id,[t])  nodeId ,SyncRepository t a Id, NTrans t a ) =>
                      Cluster  t a nodeId id
                        | nodeId-> t, nodeId-> a, nodeId-> id
                         ,t-> nodeId
                         ,a -> nodeId, t-> a, a -> t
                         ,id -> t, id-> a, id-> nodeId


  where
  --tvMyNode :: TVar a
  --tvNodes :: MVar [TVar a]
  reconnectAction :: Id -> STM(Id, [a])
  initCluster :: String -> Maybe String -> Int -> String -> IO nodeId
  queueTrans ::   [t] -> STM id
  applyCSTrans ::    STM id
  --polling :: IO (t)
  transToSend ::   Int -> Id  -> STM [t]

  receive :: (nodeId,Id,Id,[t])  -> STM(ClusterData,ClusterData,TMVar IDynamic,Id)
  respond :: (ClusterData,ClusterData,TMVar IDynamic,Id) -> STM(nodeId,Id,Id,[t])

  receiveTrans :: (nodeId,Id,Id,[t])  -> IO (nodeId,Id,Id,[t])

  -- if the node has received nothing from setReceiver, send it preempively
  sendTranss :: Int -> IO nodeId


