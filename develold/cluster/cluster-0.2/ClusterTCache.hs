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


getResource :: T.IResource (Data1 op a)=> Data1 op a ->IO (Maybe (Data1 op a))
getResource= T.getResource

getResources :: T.IResource (Data1 op a)=> [Data1 op a] ->IO [Maybe (Data1 op a)]
getResources= T.getResources

withResources ::(T.IResource (Data1 op a))=> [Data1 op a]->([Maybe (Data1 op a)]->[Data1 op a])->IO ()
withResources= T.withResources

withResource :: (T.IResource (Data1 op a))=> Data1 op a->(Maybe (Data1 op a)->Data1 op a)->IO ()
withResource= T.withResource

insertResources:: T.IResource (Data1 op a) =>[Data1 op a] -> IO ()
insertResources rs= T.withResources [] (\_->rs)

cleanTimes= 100  -- every 100 transactions the AllObjects list in memory will remove duplicate keys

type ObjectKeys= String

data Tr op=  Tr Id [ObjectKeys] op deriving (Read, Show)

data Op op a= Noop | AddObjects [Data1 op a] | ReqAddNode URIHold Id | AddNode URIHold Id |  Op op  deriving (Read, Show)

data Data1 op a = Cluster (M.Map String Node) NNodes 
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

  mapping Noop = \_ ->[]
  
  mapping (AddObjects as) = \_ ->   as   

  mapping (ReqAddNode uri id) = reqAddNode uri id where
              --create a own AddNode transaction to be spread along the cluster
              reqAddNode uri id [Just (MyNewTrans ts)]= [MyNewTrans (Tr ugenIdNode ["Cluster"](AddNode uri id):ts)]   `debug` "reqAddNode"
              reqAddNode uri id [Nothing]             = [MyNewTrans [Tr ugenIdNode ["Cluster"](AddNode uri id)]]      `debug` "reqAddNode"
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

   serialize x= show x              
   deserialize x= read x
   defPath _ = "clusterData/"

instance (Read op, Show op,Read a, Show a) => T.IResource (Op op a) where
    serialize x= show x
    deserialize str= read str
    defPath _ = "transactions/"
    keyResource _= error "not defined key for Op"

class GetProto  a where
     getProto  :: String -> a
instance (GetProto  a) => GetProto  (Data1 op a) where
        getProto  "Cluster"        = Cluster undefined undefined
        getProto  "MyNewTrans"     = MyNewTrans undefined
        getProto  "PendingTrans"   = PendingTrans undefined 
        getProto  "AllObjects"     = AllObjects undefined undefined
        getProto  str              = Data1 $ getProto  str

instance (T.IResource op, TransMap (Op op a) (Data1 op a), GetProto  (Data1 op a),
          T.IResource  (Data1 op a),
          Trans (Tr(Op op a)) (Data1 op a) Id)  
          => SyncRepository (Tr (Op op a)) (Data1 op a) Id  
   where
   addTrans t= do
        id <- genIdNode 
        withResource (MyNewTrans undefined) doit where
             doit (Just (MyNewTrans ts :: Data1 op a))   =       MyNewTrans (t:ts)
             doit Nothing= MyNewTrans [t]

 --getMyNewTrans :: id -> IO [t]  -- get Transs produced locally since last send
   getMyNewTrans id= do
      mnt <- getResource $ MyNewTrans [] :: IO (Maybe (Data1 op a))
      case mnt of
        Nothing -> return ([],[])
        Just (MyNewTrans ts) -> do
          let (t1,t2)= partition (\t-> getId t<= id) ts 
          return ({-reverse-} t1,t2)
                
          
   isAlone _= do
        initialized <- clusterInitialized
        if not initialized then return True else do
          Cluster m _ <- atomically $ readTVar tv   :: IO (Data1 op a) 
          return $ not $ M.fold (||) False $ M.map active m   

               
    --setMyNewTrans :: [t] -> IO ()
   setMyNewTrans ts= insertResources [MyNewTrans ts :: (Data1 op a)]     `debug` "setMynewTranss"
       

   emptyTranss= do
             id <- genIdNode                                             `debug` "emptyTranss getNode"               
             return $ Tr id []  Noop                                     `debug` "emptyTranss return"          

   
   applyTrans ts= mapM_ apply1 ts  where 
        apply1 t@(Tr _ keys op)=
           withResources (AllObjects undefined undefined:map (getProto  :: String -> (Data1 op a)) keys) (apply2 t) 

        apply2 t (allobjects:objs)=(AllObjects n xs):outobjs             `debug` ("**applyTrans*** "++concatMap T.serialize outobjs)

           where
           -- XXX clean AllObjects every N steps
           (n,xs)= case allobjects of
                 Nothing ->(0,keys1)
                 Just (AllObjects n' xs') ->if n' `mod` cleanTimes ==0 then  (nn,nub l) else (nn,l) where l= keys1++xs';nn= n'+ length keys1
           keys1= map T.keyResource outobjs  

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



tvio ::(T.IResource (Data1 op a)
           )  =>  IO (TVar (Data1 op a))
tvio=  reference $ (Cluster M.empty 0    ::Data1 op a)                    

tv  ::(T.IResource (Data1 op a)
           )  =>  TVar (Data1 op a)
tv= unsafePerformIO $ tvio

reference ::  (T.IResource (Data1 op a)
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
        Nothing -> return False
        Just Node{active=x} -> return x
     
   
  setActive (URIHold surl url)= atomically $ do
    Cluster map m <- (readTVar tv ::  STM (Data1 op a))                            `debug` ("setting active node"++surl)
    case M.lookup surl map of                                        
        Nothing -> return  ()                                                      `debug` ("not found in setActive in map:"++show map)
        Just (Node n uri ls lr active)-> do
            let map' = M.insert surl (Node n uri ls lr True) map
            writeTVar tv $ (Cluster map' m ::Data1 op a)                           `debug` "setted active"
    
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
  addNodeAction ts= forkIO  proc >>return() where
    proc= do     
     --print ""  `debug` ("applying "++ show ts)
     threadDelay 2000000
     case filter  isAddNode  ts of
        [] -> return ()
        xs -> do 
            
           iamTheOldestActive <-isoldactive
           print ""                                                                         `debug` ("oldestActive="++ show iamTheOldestActive)
           if not iamTheOldestActive then return () else do
            all <- allObjects
            mapM_ (process all) xs                 
            return ()                                         --`debug` ("addnodeAction done for:"++ show xs)
        where 
        process all (Tr _ _ (AddNode uri id)) | id==id0   =  sendAllObjects all uri   `debug` "sendAllObjects"
        process all (Tr _ _ (AddNode uri id ))| otherwise =  error "ERROR: to add modes with incomplete databases not implemented"

        isAddNode :: Tr(Op op a) -> Bool
        isAddNode (Tr _ _ (AddNode _ _)) = True                                                `debug`   "******AddNode found****"       
        isAddNode _ = False


        --read all the objects
        allObjects :: IO [Data1 op a]
        allObjects=do
           mall <- getResource $ AllObjects undefined undefined ::  IO (Maybe (Data1 op a))
           case mall of
            Nothing -> return []
            Just (AllObjects n all) ->   do
               mynode <- readMVar mynodem
               mallObjects <- getResources $ map getProto (( nub all) \\ ["MyNewTrans"]) ::IO [Maybe(Data1 op a)]
               return $ map (f mynode) mallObjects 
           where 
           f mynode(Just (Cluster m n))=  Cluster (M.map activate m) n 
                       where 
                       activate (Node n a b c  ac)= Node n a b c   $  if n==mynode then True else ac
                 
           f _ (Just x) = x 
           f _ Nothing  = error "sendAllObjects: object not found"


        sendAllObjects :: [Data1 op a] -> URIHold ->IO()
        sendAllObjects all uri= do
            active <- isActive uri                                                         `debug` ("uri="++ let URIHold url _= uri in  url)
            print ""                                                                       `debug` ("active="++ show active)
            if not active then return () else do
                --threadDelay 10000000
                id <- genIdNode  
                send [Tr id [] $ AddObjects all ::  Tr (Op op a)] uri
                return()
            
        isoldactive=do
              Cluster map _ <- atomically $ readTVar tv :: IO (Data1 op a)
              let clusternodes=  M.elems map
              NodeNumber mynode <- readMVar mynodem
              -- if not $ active $ clusternodes !! mynode then return False else do
              let subnodes = take mynode  clusternodes
              -- if no active node with id lower than this, then is the oldest
              if null $ filter (\Node{ active = x}-> x) subnodes  then return True else return False
   

  initCluster myurl clusterurl= do 
        tvio  :: IO (TVar(Data1 op a))
        T.clearSyncCacheProc (T.refcache :: T.Cache (Data1 op a)) 10 T.defaultCheck 10
        let myuri= makeURI myurl
        runCluster  10 (uriPort1 myuri)   --seconds
        --if myurl==clusterurl then threadDelay 100000000 else return ()  -- XXX test
        print "end delay"
        let clusteruri= makeURI clusterurl
        send [Tr id0 ["MyNewTrans"] (ReqAddNode  myuri id0):: Tr (Op op a)]  clusteruri >> return () --send a request to a node of the cluster
        threadDelay 1000000

        atomically $ do   -- wait until the node has been created
            Cluster _ nnodes <- readTVar tv :: STM (Data1 op a)
            if nnodes==0 then retry                                             `debug` "no node" 
                         else return ()                                         `debug` "NODE CREATED"


        n <-  setMyNode myuri                                                   `debug` "setMyNode"
        modifyMVar_ mynodem (\_->return $  NodeNumber n )                       `debug` "takeMVar mynodem"
        print "" `debug` "DESPUES DE TAKE"
        return myuri                                                            `debug` "return"
        
{-applyCSTrans ::   [t] -> IO ()
  applyCSTrans  ts = do
     let undefined= u
     toApply <- withResources_ [ Cluster u u, MyNewTrans u, PendingTrans u, AllObjects u u] doit
     appyTrans toApply
     addNodeAction ts1                                                             

     where
     doit[Just (Cluster map n),Just (MyNewTrans nts), Just (PendingTrans ts'), Just (AllObjects alls n)]=
      let
         nodeIds = [u | (Node _ u _ _ _) <- M.elems map]                                                          `debug`  "in applyCSTrans: "
         id = getConsolidatedId nodeIds
                                                          -- foreigh pending transactions
         (ts'',tsnew)<- partition (\t-> getId t<= id) nts
         alone <- not $ M.fold (||) False $ M.map active map                                                           `debug` "in isAlone"
         all= if alone then ts'' ++ts' ++ ts ++ tsnew else ts'' ++ts' ++ ts        
         tss =  sortBy compareIds all

         let (ts1,ts2) = if alone then (tss,[]) else partition (\t-> getId t<= id) tss
      in
         ([MynewTrans $ if alone then [] else tsnew , PendingTrans ts2], ts1)                                                         `debug` ("alone="++show alone)

     
     -- se ha recibido todas las transacciones menores que id
     getConsolidatedId :: [nodeId] -> IO Id
     getConsolidatedId []= return id0
     getConsolidatedId nodeIds=  mapM getLastIdReceived nodeIds >>= return . maximum
 
     compareIds t t'= compare (getId t) (getId t')
     
-}

 
errors str=  error $ "url not found in cluster: "++ str



 
addTransaction :: Cluster  (Tr (Op op a)) (Data1 op a) URIHold  =>[String] -> op  -> IO ()
addTransaction keys op  =  createTrans keys op >>= addTrans 

createTrans :: Cluster  (Tr (Op op a)) (Data1 op a) URIHold => [String] ->  op  -> IO (Tr (Op op a))
createTrans keys op = do
    id <- genIdNode                              
    return $ Tr id keys  (Op op)
    
genIdNode = do
    id     <- genId
    mynode <- readMVar mynodem
    return   id{node= mynode}

ugenIdNode = unsafePerformIO genIdNode 
         

        

