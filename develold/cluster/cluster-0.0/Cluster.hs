{-# OPTIONS  -fglasgow-exts  #-}
module Cluster where

import Control.Concurrent
import System.IO.Unsafe
import Data.List
import System.Time
import Transaction
import Debug.Trace
import Control.Monad

debug a b= trace b a



class Trans t  a id  => SyncRepository t a id   where
   getMyNewTrans :: id -> IO ([t],[t])  -- get Transaction produced locally since last send
   setMyNewTrans :: [t] -> IO ()
   applyTrans :: [t] -> IO ()
   getPendingTransactions :: IO [t]
   setPendingTransactions :: [t]-> IO ()
   addTrans :: t -> IO ()

data CClockTime= TTOD Integer Integer deriving (Read, Show, Eq, Ord)
         
-- transactions where the id of the transactions include the  node number in the cluster
data NodeNumber= NodeNumber Int deriving (Eq,Ord, Read, Show)

data Seq = Seq Int deriving (Eq,Ord, Read, Show)

data Id= Id {time::CClockTime,seq :: Seq, node:: NodeNumber}  deriving (Eq,Read,Show)

id0= Id (TTOD 0 0) (Seq 0) (NodeNumber 0)

genId= do liftM3 Id getTime genSeq  (return (NodeNumber 0))
          
getTime=   do
            TOD t1 t2 <-getClockTime
            return $ TTOD t1 t2
            
seqVar:: MVar Int
seqVar= unsafePerformIO $ newMVar 0

genSeq= liftM Seq  $ withMVar seqVar (\x -> return (x+1))
          

   

instance Ord Id where
  compare (Id t s n) (Id t' s' n')= 
           case compare t t' of
              LT -> LT
              GT ->GT
              EQ -> case compare n n' of
                    LT -> LT
                    GT -> GT
                    EQ ->  compare s s'


class Trans t a Id => NTrans t a  where
    getNode :: t -> Int


class Protocol t nodeId | t ->nodeId, nodeId -> t where
   send :: [t] -> nodeId -> IO (Maybe [t])
   setCallback :: Int -> ([t] -> nodeId -> IO [t]) ->IO()

--tratar cluster como un objeto mas del repositorio sometido a transacciones
--asi se trata mejor las adiciones y borrados de nodos al/del cluster
class (Protocol t nodeId ,SyncRepository t a Id, NTrans t a ) => 
                      Cluster  t a nodeId | nodeId->t, nodeId->a, t->nodeId, a -> nodeId, t->a, a -> t where
  
  
  setMyNode :: nodeId -> IO Int
  getNodeIds :: IO [nodeId]
  isActive ::  nodeId -> IO Bool
  setActive :: nodeId -> IO ()
  --setPassive
  --getNodeNumber ::  nodeId -> Int
  
  getLastIdSent :: nodeId -> IO Id       
  putLastIdSent :: nodeId -> Id -> IO()

  getLastIdReceived :: nodeId -> IO Id  
                            
  putLastIdReceived :: nodeId ->Id ->IO()

  
  initCluster :: String -> String ->  IO nodeId
  
  runCluster ::  Int -> Int -> IO [nodeId]
  runCluster interval port= do
      forkIO $ setCallback port (receiveTrans ::[t] -> nodeId -> IO [t]) 
      threadDelay 1000000
      forkIO $ sendLoop interval (sendTranss ::IO nodeId)
      getNodeIds
      where
      sendLoop interval f= do 
                 f
                 threadDelay (fromIntegral $ interval * 1000000)                    `debug` "loop"
                 sendLoop interval f

  addNodeAction ::[t]-> IO()    -- to detect and process transactions that add nodes to the cluster
  
  applyCSTrans ::   [t] -> IO ()
  applyCSTrans  ts = do

     nodeIds <- getNodeIds                                                             `debug`  "in applyCSTrans: "
     id <- getConsolidatedId  nodeIds
     ts' <- getPendingTransactions
     (ts'',tsnew)<- getMyNewTrans id
     let tss = sortBy compareIds (ts'' ++ts' ++ ts)
     let (ts1,ts2) = if id== id0 then (tss,[]) else partition (\t-> getId t<= id) tss
     applyTrans ts1
     addNodeAction ts1       
     setPendingTransactions ts2
     if null ts then setMyNewTrans tsnew else return()

     where
       -- se ha recibido todas las transacciones menores que id
     getConsolidatedId :: [nodeId] -> IO Id
     getConsolidatedId []= return id0
     getConsolidatedId nodeIds=  mapM getLastIdReceived nodeIds >>= return . maximum
 
     compareIds t t'= compare (getId t) (getId t')
     


  receiveTrans :: [t] -> nodeId -> IO [t]        
  receiveTrans ts node = do
     applyThem node (getNode $ head ts) ts
     
            
     where
     applyThem node nodenumber ts= 
        case nodenumber of
               0 -> 
                    -- new node. head ts must be a Transaction over the node list to add a new node to the cluster.
                    -- that must propagate this transaction to the rest of the nodes
                    -- once added to the list of nodes, the rest of the nodes will forward all the transactions
                    -- here authentication of requests are necessary in orde to avoid fake nodes
                    applyThem1 node ts
                    
                    
               _ ->do  active <- isActive node 
                       case active of
                         False ->do
                            setActive node  --ode has been disconected for a while
                            (applyThem1 node . filter commutative) ts  -- only commutative transactions are applied 
                                                                       -- from offline nodes
                         True -> applyThem1 node ts
        
        where
        applyThem1 node ts= do
            lastId <- getLastIdReceived node
            let ts'= if lastId  == id0 then ts else dropWhile (\t-> getId t <= lastId) ts  --to avoid to apply already processed 
                                                                                           --transactions coming from this node
            putLastIdReceived node (getId $ last ts)
            applyCSTrans ts'
            if lastId == id0 then return [] else do
                        id      <- getLastIdSent node 
                        (_,myTs) <- getMyNewTrans id             --send the transactions not already sent to this node.
                        case myTs of
                                [] -> return []
                                _  -> do putLastIdSent node (getId $ last myTs)  
                                         return myTs


  sendTranss :: IO nodeId
  sendTranss= do
     nodeIds <- getNodeIds                                                           `debug` "sendTranss"
     
     flags <- mapM  send1  nodeIds
     if foldr  (||) False flags then return ()  -- all transactions processed
         else applyCSTrans  []  --to process local transactions even if no received transactions or no cluster
     return $ head nodeIds 
     
     where

     send1 nodeId= do
        active <- isActive nodeId
        case active of
         True -> do
                id <- getLastIdSent nodeId
                (_,ts') <- getMyNewTrans id
                case ts' of
                  [] -> return True  `debug`"nada"
                  _  -> do sendT ts' nodeId
                           return True 
                        
         False -> return False
                
     sendT:: [t] -> nodeId -> IO () 
     sendT ts node = do
       mts <- send ts node
       case mts of
          Just ts'  -> do 
                   putLastIdSent node (getId $ last ts) 
                   putLastIdReceived node (getId $ last ts')
                   applyCSTrans  ts'            
                   return ()
                   
          Nothing -> return ()
     
    




   

