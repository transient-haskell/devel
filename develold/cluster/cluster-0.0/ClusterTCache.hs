{-# OPTIONS  -fglasgow-exts -fallow-undecidable-instances  #-}
module ClusterTCache where
import Transaction
import Cluster
import URIHold
import ProtocolHTTP
import qualified Data.TCache as T
import Data.Maybe(fromJust,mapMaybe)


import System.IO.Unsafe
import Control.Concurrent.STM 
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map as M
import Data.List(partition,nub,find,(\\))


getTVars :: T.IResource (Data1 op a)=> [Data1 op a] -> IO [Maybe (TVar (Data1 op a))] 
getTVars= T.getTVars

insertResources:: T.IResource (Data1 op a) =>[Data1 op a] -> IO ()
insertResources= T.insertResources

getResource :: T.IResource (Data1 op a)=> Data1 op a ->IO (Maybe (Data1 op a))
getResource= T.getResource

getResources :: T.IResource (Data1 op a)=> [Data1 op a] ->IO [Maybe (Data1 op a)]
getResources= T.getResources

withResources ::(T.IResource (Data1 op a))=> [Data1 op a]->([Maybe (Data1 op a)]->[Data1 op a])->IO ()
withResources= T.withResources

withResource :: (T.IResource (Data1 op a))=> Data1 op a->(Maybe (Data1 op a)->Data1 op a)->IO ()
withResource= T.withResource


cleanTimes= 100  -- every 100 transactions the AllObjects list in memory will remove duplicate keys

type ObjectKeys= String

data Tr op= Tr Id [ObjectKeys] op deriving (Read, Show)

data Op op a= AddObjects [a] | ReqAddNode URIHold Id | AddNode URIHold Id |  Op op  deriving (Read, Show)

data Data1 op a =  Cluster (M.Map String Node) NNodes 
                | MyNewTrans [Tr(Op op a)] 
                | AllObjects Int [String]
                | PendingTrans [Tr(Op op a)] 
                | Data1 a 
                deriving (Read, Show)
     
type NNodes= Int


data Node= Node{number::NodeNumber,uri:: URIHold, lastSent::Id, lastReceived:: Id, active:: Bool} deriving(Read,Show)

-- the mapping associate a transformation of objects from the operation codes received
class TransMap op a | op ->a, a ->op  where
  mapping :: op -> ([Maybe a]-> [a])
  commutativeOp :: op -> Bool
  commutativeOp op= True

instance TransMap op a => TransMap  (Op op a) (Data1 op a)  where

  mapping (AddObjects as) = \_ ->  map Data1 as   

  mapping (ReqAddNode uri id) = reqAddNode uri id where
              --create a own AddNode transaction to be spread along the cluster
              reqAddNode uri id [Just (MyNewTrans ts)]= [MyNewTrans (Tr ugenIdNode ["Cluster"](AddNode uri id):ts)]   `debug` "reqAddNode"
              reqAddNode uri id [Nothing]= [MyNewTrans [Tr ugenIdNode ["Cluster"](AddNode uri id)]]                   `debug` "reqAddNode"
              reqAddNode _ _ v = error $ "error not expected "

  mapping (AddNode uri id) = addNode uri id where
          addNode :: URIHold -> Id -> [Maybe (Data1 op a)] -> [Data1 op a]
          addNode uri@(URIHold surl url) id  [Just (Cluster map nNodes)]                                   
                 =   [Cluster (M.insert surl (Node (NodeNumber nNodes) uri id0 id True) map) (nNodes+1)]  `debug` ("*****addNode*****")
                -- XXX check that this node has not been already added !!!
          addNode _ _ _= error "cluster don't exist"

          --DelNode uri1 uri2 id -> delNode uri1 uri2 id

  mapping (Op op) = f $ mapping op where          
          f f1 dmas =  map (\a-> Data1 a) as where
                 as= f1 mas  
                 mas= map filter dmas 
                 filter (Just (Data1 a))= Just a 
                 filter Nothing = Nothing

instance  TransMap  op a => Trans (Tr (Op op a)) (Data1 op a) Id where
  --apply:: t -> [Maybe a] -> IO [a]
  apply (Tr _ _ op) mas = (mapping op) mas                                                                        `debug` "apply"

  --getId:: t -> id
  getId (Tr id _ _)= id

  commutative (Tr _ _ (ReqAddNode _ _)) = False  
  commutative (Tr _ _ (AddNode _ _)) = False
  commutative (Tr _ _ (AddObjects  _)) = False
  commutative (Tr _ _ (Op op)) = commutativeOp op

  
instance TransMap op a => NTrans (Tr (Op op a)) (Data1 op a)  where
  --getNode :: t -> Int
  getNode t= node where Id _ _ (NodeNumber node)= getId t







{-
proceso de conexion: se envia un addnode a un nodo del cluster que lo reenvia a los demás . el addnode se ejecuta
como otra transaccion cualquiera, cuando le toca su tiempo.

desconexión forzada: un nodo no recibe comunicación de otro. envia un delNode, su URL la del que no comunica y mas antiguo
de los lastIdReceived de todos los nodos, que logicamente será del nodo que falla.
cada nodo que recibe compara su lasIdReceived


delNode :: URIHold -> Id -> [Maybe (Data1 op a)] -> [Data1 op a]
delNode uri1@(URIHold url1) uri2@(URIHold url2) id [Just (Cluster map nNodes)]=
   id0= unsafePerformIO getLastIdReceived
   case compare id0 id of
     EQ -> [Cluster (M.delete (toString url2)  uri id0 id) map) nNodes]
     GT -> [Cluster (M.delete (toString url2)  uri id0 id) map) nNodes]
-}

          





instance (Show op, Show a, Read op, Read a,T.IResource a) => T.IResource (Data1 op a )  where 
   keyResource (Cluster _ _)    = "Cluster"
   keyResource (MyNewTrans _)   = "MyNewTrans"
   keyResource (PendingTrans _) = "PendingTrans"
   keyResource (AllObjects _  _)   = "AllObjects"
   keyResource (Data1 a)         = T.keyResource a


instance (Read op, Show op, Read a, Show a) => T.Serializable (Data1 op a) where
  serialize x= show x              
  deserialize x= read x
  --defPath _ = "clusterData/"

instance (Read op, Show op,Read a, Show a) => T.Serializable (Op op a) where
    serialize x= show x
    deserialize str= read str
    --defPath _ = "transactions/"

class GetProto  a where
     getProto  :: String -> a
instance (GetProto  a) => GetProto  (Data1 op a) where

        getProto   "Cluster"       = Cluster undefined undefined
        getProto   "MyNewTrans"    = MyNewTrans undefined
        getProto  "PendingTrans"   = PendingTrans undefined 
        getProto  "AllObjects"     = AllObjects undefined undefined
        getProto  str              = Data1 $ getProto  str

   

instance ( T.Serializable op,TransMap (Op op a) (Data1 op a), GetProto  (Data1 op a),
          T.IResource  (Data1 op a),
          Trans (Tr(Op op a)) (Data1 op a) Id)  
          => SyncRepository (Tr (Op op a)) (Data1 op a) Id  
   where
   addTrans (Tr id keys op)= do
        id <- genIdNode 
        withResource (MyNewTrans undefined) doit where
             doit (Just (MyNewTrans ts :: Data1 op a))   =       MyNewTrans (Tr id keys op:ts)
             doit Nothing= MyNewTrans [Tr id ["MyNewTrans"] op]

   --getMyNewTrans :: id -> IO [t]  -- get Transs produced locally since last send
   getMyNewTrans id= do
      mnt <- getResource $ MyNewTrans [] ::IO (Maybe (Data1 op a))
      case mnt of
        Nothing -> return ([],[])
        Just (MyNewTrans ts) -> 
          case id of
           id0 -> return (ts,[])
           _ -> return $ partition (\t-> getId t<= id) ts 
                    
    --setMyNewTrans :: [t] -> IO ()
   setMyNewTrans ts= insertResources [MyNewTrans ts :: (Data1 op a)]
       

  
   applyTrans ts= mapM_ apply1 ts  where 
        apply1 t@(Tr _ keys op)=
           withResources (AllObjects undefined undefined:map (getProto  :: String -> (Data1 op a)) keys) (apply2 t) 
                     
        apply2 t (allobjects:objs)=(AllObjects n xs):outobjs                                      `debug` "**applyTrans***"
        
           where
           -- XXX clean AllObjects every N steps
           (n,xs)= case allobjects of
                 Nothing ->(0,keys1)
                 Just (AllObjects n' xs') ->if n' `mod` cleanTimes ==0 then  (nn,nub l) else (nn,l) where l= keys1++xs';nn= n'+1
           keys1= map T.keyResource dataoutobjs where dataoutobjs= filter f outobjs where f(Data1 _)=True;f _= False
           outobjs :: [Data1 op a]
           outobjs= apply  t objs
   
  
               

   --getPendingTransactions :: IO [t]
   getPendingTransactions= do
      mpts <- getResource $ PendingTrans []  :: IO (Maybe(Data1 op a))
      case mpts of
        Nothing -> return []
        Just (PendingTrans ts) -> return ts

   --setPendingTransactions :: [t]-> IO ()
   setPendingTransactions ts= 
      insertResources ([PendingTrans ts :: Data1 op a] )  



tvio ::(T.IResource (Data1 op a),
           Read a,
           Read op,
           Show a,
           Show op
           )  =>  IO (TVar (Data1 op a))
tvio=  reference $ (Cluster M.empty 0    ::Data1 op a)                    

tv  ::(T.IResource (Data1 op a),
           Read a,
           Read op,
           Show a,
           Show op
           )  =>  TVar (Data1 op a)
tv= unsafePerformIO $ tvio

reference ::  (T.IResource (Data1 op a),
           Read a,
           Read op,
           Show a,
           Show op
           )  =>  Data1 op a -> IO (TVar (Data1 op a)) 
reference x=do
             mv <- getTVars [x ]
             case mv of
               [Nothing] -> do 
                       insertResources [x]
                       reference x
                         
               [Just cl] -> return cl



instance  (Show a, Read a, Show op, Read op
          ,Protocol (Tr (Op op a)) URIHold 
          ,SyncRepository (Tr (Op op a)) (Data1 op a) Id 
          ,T.IResource (Data1 op a)
          ,TransMap op a
          --,TransMap  (Op op) (Data1 op a)
          ,GetProto  a
          ,NTrans (Tr (Op op a)) (Data1 op a) ) 
          => Cluster  (Tr (Op op a)) (Data1 op a) URIHold  where

  setMyNode (URIHold surl _)= atomically $ do             --mark my node as inactive (don't send messages to myself)
    Cluster map m <-  readTVar tv :: STM (Data1 op a)
    let node = case M.lookup surl map of
            Nothing -> errors surl
            Just node -> node 
    let Node{number=NodeNumber x} = node
    let map' = M.insert surl node{active=False} map
    writeTVar tv $ (Cluster map' m :: Data1 op a)
    return x
       
  getNodeIds = do
       Cluster map _ <- atomically $ readTVar tv :: IO (Data1 op a)
       return [u | (Node _ u _ _ _) <- M.elems map]

             
             
  
  isActive (URIHold surl url)= do
    Cluster map _ <- atomically $ readTVar tv ::  IO (Data1 op a)
    case M.lookup surl map of
        Nothing -> errors surl
        Just Node{active=x} -> return x
     
   
  setActive (URIHold surl url)= atomically $ do
    Cluster map m <- readTVar tv ::  STM (Data1 op a)
    case M.lookup surl map of
        Nothing -> errors  surl
        Just (Node n uri ls lr active)-> do
            let map' = M.insert surl (Node n uri ls lr True) map
            writeTVar tv $ (Cluster map' m ::Data1 op a)
    
  --getLastIdSent :: nodeId -> IO Id
  getLastIdSent (URIHold surl url)= do
    Cluster map _ <- atomically $ readTVar tv ::  IO (Data1 op a)
    case M.lookup surl map of
        Nothing -> return   id0
        Just (Node _ _ lastSend _ _) -> return lastSend
     
  putLastIdSent (URIHold surl url) id= atomically $ do  -- :: nodeId -> Id -> IO()
    Cluster map m <- readTVar tv ::  STM (Data1 op a)
    case M.lookup surl map of
        Nothing -> errors surl
        Just (Node n uri ls lr st)-> do
           let map'= M.insert surl (Node n uri id lr st) map
           writeTVar tv $ (Cluster map' m :: Data1 op a)

             
  getLastIdReceived (URIHold surl url) = do -- :: nodeId -> IO Id  
     Cluster map _ <- atomically $ readTVar tv :: IO (Data1 op a)
     case M.lookup surl map of
        Nothing -> return id0
        Just (Node _ _ lastSend _ _) -> return lastSend                         

 
  putLastIdReceived (URIHold surl url) id
   | id ==id0= return ()  --new node
   | otherwise=
            atomically $ do -- :: nodeId ->Id ->IO()
             Cluster map m <- readTVar  tv :: STM (Data1 op a)
             case M.lookup surl map of
                Nothing -> errors  surl
                Just (Node n uri ls lr st)-> do
                   let map'= M.insert surl (Node n uri ls id st) map
                   writeTVar tv $ (Cluster map' m :: Data1 op a)

  
  --addNodeAction ::[t]-> IO()    -- to detect and process transactions that add nodes to the cluster
  addNodeAction ts= do
   case filter  isAddNode  ts of
        [] -> return()
        xs -> do 
           all <- allObjects
           mapM_ (process all) xs
        where 
        
        process all (Tr _ _ (AddNode uri id0)) =  print "ERROR: to add modes with incomplete databases not implemented"
        process all (Tr _ _ (AddNode uri id )) =  sendAllObjects all uri

        isAddnode :: Tr(Op op a) -> Bool
        isAddnode (Tr _ _ (AddNode _ _)) = True                                                `debug`   "******AddNode found****"       
        isAddNode _ = False

        sendAllObjects :: [a] -> URIHold ->IO()
        sendAllObjects all uri= do
               id <- genIdNode 
               send [Tr id [] $ AddObjects all ::  Tr (Op op a)] uri
               return()

        --read all the objects
        allObjects :: IO [a]
        allObjects=do
           mall <- getResource $ AllObjects undefined undefined ::  IO (Maybe (Data1 op a))
           case mall of
            Nothing -> return []
            Just (AllObjects n all) ->   do
             iamTheOldestActive <-isoldactive
             if not iamTheOldestActive then return [] else do
               mallObjects <- getResources $ map getProto $ nub all ::IO [Maybe(Data1 op a)]
               return $ map f mallObjects 
           where 
           f (Just(Data1 x))= x 
           f Nothing= error "sendAllObjects: object not found"
               
               
               
        isoldactive=do
              Cluster map _ <- atomically $ readTVar tv :: IO (Data1 op a)
              let clusternodes=  M.elems map
              NodeNumber mynode <- readMVar mynodem
              if not $ active $ clusternodes !! mynode then return False else do
                 let subnodes = take mynode  clusternodes
                 if null $ filter (\Node{ active = x}-> x) subnodes  then return False else return True
              
  initCluster myurl clusterurl= do 
        tvio  :: IO (TVar(Data1 op a))
        --T.clearSyncCacheProc (T.refcache :: T.Cache (Data1 op a)) 10 T.defaultCheck 10
        let myuri= makeURI myurl
        runCluster  10 (uriPort1 myuri)   --seconds
        --if myurl==clusterurl then threadDelay 100000000 else return ()  -- XXX test
        print "end delay"
        let clusteruri= makeURI clusterurl
        send [Tr id0 ["MyNewTrans"] (ReqAddNode  myuri id0):: Tr (Op op a)]  clusteruri >> return () --send a request to a node of the cluster
        threadDelay 1000000
        
        atomically $ do   -- wait until the node has been created
            Cluster _ nnodes <- readTVar tv :: STM (Data1 op a)
            if nnodes==0 then retry else return ()
        n <-  setMyNode myuri --set th
        takeMVar mynodem
        putMVar mynodem $ NodeNumber n
        return myuri
        


 
errors str=  error $ "url not found in cluster: "++ str



 
addTransaction :: Cluster  (Tr (Op op a)) (Data1 op a) URIHold  =>[String] -> op  -> IO ()
addTransaction keys op  =  createTrans keys op >>= addTrans 

createTrans :: Cluster  (Tr (Op op a)) (Data1 op a) URIHold => [String] ->  op  -> IO (Tr (Op op a))
createTrans keys op = do
    id <- genIdNode 
    return $ Tr id keys  (Op op)
    
genIdNode = do
    id <- genId
    mynode <- readMVar mynodem
    return   id{node= mynode}

ugenIdNode = unsafePerformIO genIdNode 
         
mynodem :: MVar NodeNumber
mynodem= unsafePerformIO $ newMVar (NodeNumber 0)



