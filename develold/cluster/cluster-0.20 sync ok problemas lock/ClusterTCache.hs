{-# OPTIONS  -fglasgow-exts -fallow-undecidable-instances  #-}
module ClusterTCache where
import Transaction
import Cluster
import URIHold
import Protocol
import ProtocolHTTP hiding(debug)
import Data.TCache.Dynamic 
import Data.Typeable
import Data.Maybe(isJust,fromJust,mapMaybe, catMaybes)
import Control.Exception(handle, finally)
import Data.List (sortBy,nubBy,dropWhile,deleteBy)
import Data.HashTable(hashString)

import System.IO.Unsafe
import Control.Concurrent.STM 
import GHC.Conc	(unsafeIOToSTM)
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map as M
import Data.List(partition,nub,find,(\\),delete)
import System.Time
import Control.Exception(assert)
import Control.Monad(when, liftM)
import Data.Monoid

import Unsafe.Coerce



resources = Res [] [] [] ()
cleanTimes= 100  -- every 100 transactions the AllObjects list in memory will remove duplicate keys

type ObjectKeys= String

data Tr op=  Tr Id [ObjectKeys] op deriving (Read, Show, Eq)

data Op op a=  AddObjects [Data1 op a] | Connect | Noop  |  Op op  deriving (Read, Show, Eq)



data Data1 op a = PendingTrans [Tr(Op op a)] 
                | Key1 String
                | Data1 a 
                deriving (Read, Show, Eq,Typeable)



instance (Show op, Show a, Read op, Read a,IResource a) => IResource (Data1 op a )  where 
   keyResource (Data1 a)        = keyResource a
   keyResource (PendingTrans _) = "PendingTrans"
   keyResource (Key1 s)= s
   serialize x   = show x              
   deserialize x = read x
   defPath _ = show s++"/" where 
                           url= unsafePerformIO $ readMVar myNodeName
                           s  = uriPort1 $ makeURI url 

instance (Read op, Show op,Read a, Show a) => IResource (Op op a) where
    serialize x= show x
    deserialize str= read str
    keyResource _= error "not defined key for Op"

{-
class GetProto  a where
     getProto  :: String -> a
     
instance (GetProto  a) => GetProto  (Data1 op a) where
        getProto s@('h':'t':'t':'p':':':str) = Node nod0{uri=URIHold s undefined }
        getProto  "PendingTrans"   = PendingTrans undefined 
        getProto  "AllObjects"     = AllObjects undefined undefined
        getProto  str              = Data1 $ getProto  str
     
-}

-- the mapping associate a transformation of objects from the operation codes received
class (Eq op, Eq a)=> TransMap op a | op ->a, a ->op  where
  mapping :: op -> ([Maybe a]-> [a])
  commutativeOp :: op -> Bool
  commutativeOp op= True

instance TransMap op a => TransMap  (Op op a) (Data1 op a)  where

  mapping Connect  = \_->[  ]
  mapping Noop = \_ ->[]
  
  mapping (AddObjects as) = \_ ->   as                                 `debug` "applying AddObjects"


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


  commutative (Tr _ _ (Connect )) = False
  commutative (Tr _ _ Noop) = True
  commutative (Tr _ _ (AddObjects _ )) = False

  commutative (Tr _ _ (Op op)) = commutativeOp op

  nullTrans= undefined
  allTransactions=undefined
  applyTo _= undefined 


instance TransMap op a => NTrans (Tr (Op op a)) (Data1 op a)  where
  --getNode :: t -> Int
  getNode t= node where Id _ _  node= getId t



instance (IResource op, TransMap (Op op a) (Data1 op a),        
          IResource  (Data1 op a),
          Trans (Tr(Op op a)) (Data1 op a) Id
          ,Typeable op, Typeable a)
          => SyncRepository (Tr (Op op a)) (Data1 op a) Id  
   where
   


   addTrans t= atomically $ 
            withSTMResources[PendingTrans undefined] 
              (\[Just (PendingTrans ts)]-> 
                     Res [Just $ PendingTrans (t:ts)] [] [] () )
            --PendingTrans ts <- return . fromIDyn =<< readTVar tvPendingTrans 
            --writeTVar tvPendingTrans $ toIDyn $ PendingTrans (t:ts)

   emptyTrans= do
            id <- genIdNode                                             `debug` "emptyTranss getNode"               
            return $ Tr id []  Noop                                     `debug` "emptyTranss return"          

   applyTrans ts= mapM_ apply1 ts  where 
        apply1 t@(Tr _ keys op)= do
           AllObjects n' xs' <- return . fromIDyn =<< readTVar tvAllObjects :: STM ClusterData 

           let f= unsafeCoerce 1
           keys1 <- withSTMResources ( map  (Key1 :: String -> Data1 op a)  keys) (apply2 t)     `debug` ("applyTrans for keys="++show keys) 
           
           let (n, xs) =let l= keys1++xs'; nn= n'+ length keys1 
                        in if n' `mod` cleanTimes ==0 then  (nn,nubBy nubBy1 l) 
                                                      else (nn,l) 
                                                
           writeTVar tvAllObjects $ toIDyn $ AllObjects n xs
        
        nubBy1 (s,_)(s',_)= s==s'

        apply2 t objs= Res [] outobjs [] keys1     `debug` ("**applyTrans*** "++concatMap serialize outobjs)

           where
        
           
           keys1= map keyt outobjs  where 
                keyt r= (keyResource r,time 0)
                time x= t where TOD t _= unsafePerformIO $ getClockTime
           
           outobjs= apply t objs               `debug`("objs=" ++ concatMap serialize ( catMaybes objs))

instance IResource ClusterData where
   keyResource (Node Nod{uri=URIHold s _})    = show $ hashString s
   keyResource (MyNode _)       = "MyNode"
   keyResource (AllObjects _ _ )= "AllObjects"

   serialize x   = show x              
   deserialize x = read x
   defPath _ = show s++"/" where 
                           url= unsafePerformIO $ readMVar myNodeName
                           s  = uriPort1 $ makeURI url 


   
  
instance  (Show a, Read a, Show op, Read op
          ,Protocol (URIHold,Id,Id,[Tr (Op op a)]) URIHold 
          ,SyncRepository (Tr (Op op a)) (Data1 op a) Id 

          ,IResource (Data1 op a)
          ,Typeable (Data1 op a)
          ,TransMap op a
          ,Typeable a, Typeable op          
          ,NTrans (Tr (Op op a)) (Data1 op a) ) 
          => Cluster (Tr (Op op a)) (Data1 op a) URIHold Id where
    
      
        
            
  reconnectAction id = do
           AllObjects n all <- return . fromIDyn =<< readTVar tvAllObjects          `debug` "tvMyNode"
           let (all1,_)= unzip . filter (\(_,mod)->  mod>t) $ nubBy nubBy1 all
           
           let t= unsafeCoerce 1
           withSTMResources (map Key1 ( all1 \\ ["PendingTrans","MyNode"]) ) f 

           where 
           f mas= resources {toReturn=catMaybes mas }
           Id (TTOD t _) _ _= id
           nubBy1 (s,_)(s',_)= s==s'





  initCluster myurl mclusterurl interval= do 
        registerType :: IO ClusterData
        registerType :: IO (Data1 op a)
        --tvnodes<- readMVar tvNodes
        withResources [] (\_->[PendingTrans [] :: Data1 op a])
        
        --clearSyncCacheProc (refcache :: Cache (Data1 op a)) 10 defaultCheck 10  `debug` "initCluster"
        let myuri= makeURI myurl
        let port= uriPort1 myuri
        modifyMVar_   myNodeName (\_->  return myurl)
                          
          
        forkIO $  setReceiver port (receiveTrans ::(URIHold,Id,Id,[Tr (Op op a)])  -> IO (URIHold,Id,Id,[Tr (Op op a)])) 
        threadDelay 1000000
        forkIO $ loop interval (sendTranss :: IO URIHold)                  
        threadDelay 1000000
        forkIO $ loop interval (polling :: IO (Tr (Op op a)))

        when (isJust mclusterurl) $ do
          let clusteruri= makeURI . fromJust $ mclusterurl

          [tv] <- atomically $ getTVars[toIDyn $ Node nod0{uri=clusteruri}]

          case tv of
              Nothing -> connect myuri clusteruri 
                  
              _ -> return ()

        atomically $ do
                           MyNode nod <- return . fromIDyn =<< readTVar  tvMyNode 
                           writeTVar tvMyNode $ toIDyn $ MyNode nod{uri= myuri}
          

        return myuri                                                            `debug` "return"

        where
        connect myuri clusteruri=do
          [tv] <- getTVarsIO[toIDyn $ Node nod0{uri=clusteruri, active=ConnectSent}]     --create the other computer node
          tvnodes <- takeMVar tvNodes
          putMVar tvNodes $  tv:tvnodes
                                    

          id <- genIdNode
          mts <- send (myuri, id0, id0,[Tr id [] Connect:: Tr (Op op a)])  clusteruri --send a request to a node of the cluster
          putStrLn $ "connection response:"++ show mts
          case mts of
              Just msg  -> do
                         receive msg
                         return()
                             
            
              Nothing -> do
                 error $ "cluster at adddress "++show clusteruri++" not reached"
                 


          threadDelay 1000000
  
          atomically $ do   -- wait until the node has been connected
              MyNode nod <- return . fromIDyn =<< readTVar tvMyNode
              if not $ show clusteruri `elem` nodesConected nod 
                then retry                                             `debug` ("not conected, nodes="++ show( nodesConected nod)) 
                else return()


          return ()
          
  applyCSTrans  ts = do
        MyNode mynode <-  return . fromIDyn =<<  readTVar tvMyNode         `debug` "applyCSTrans" 
        let nodeIds = nodesConected mynode
        
        --Just tvtrans:tvnodes <-  unsafeIOToSTM $ getTVars $ PendingTrans u:[Node nod0{uri=URIHold n u}| n <- nodeIds]
        [Just tvPendingTrans] <- getTVars [toIDyn (PendingTrans undefined :: Data1 op a)]
        PendingTrans all <- return . fromIDyn =<< readTVar tvPendingTrans  :: STM (Data1 op a) 
        tvnodes <- unsafeIOToSTM $  readMVar tvNodes                       `debug` ("PendingTrans all: "++ show all)
        let f tv= readTVar tv >>= \d -> return $ fromIDyn d
        nodes <-  mapM f  tvnodes 
        let firstValid []= lastReceived mynode
            firstValid (t:ts)= let id= getId t in if id /=id0 then id0 else firstValid ts
        let lastRec= firstValid ts
        --let lastRec=  if not $ null ts then getId $ last ts else lastReceived mynode
        let oldconsid= consolidatedId mynode

        let consId = case length nodes `debug` ("nodes="++show nodes)of
              0 -> lastRec
              1 -> maximum [lastRec, consolidated] where 
                        consolidated=consolidatedId $ node
                        Node node= head nodes 
              _ -> let consIdsn=  --map (\(Node node) -> lastReceived node) nodes ++
                                  map (\(Node node) -> consolidatedId node) nodes 
                       consIds= delete id0 $ nub consIdsn
                   
    
                   in  if null consIds then lastRec else minimum  consIds   `debug` ("consids="++show consIds)
               
        let eqID x y= getId x==getId y
        let tss =  nubBy eqID $ sortBy compareIds (all ++ ts)                      `debug` ("consId= "++show consId)
        let (ts1,ts2) =  if null $ nodesConected mynode
                         then (tss,[])
                                else span (\t-> getId t<= consId) tss
                         
        ts3<- case (ts2,(oldconsid /= consId)) of
              ([],True) -> do
                      id <- unsafeIOToSTM $ genIdNode
                      return [Tr id [] Noop]                 -- to forward the new consId to other nodes
              _ -> return ts2

        writeTVar tvPendingTrans $ toIDyn $ PendingTrans ts3                          `debug` ("PendingTrans: "++ show ts3)
        writeTVar tvMyNode $ toIDyn $ MyNode mynode{consolidatedId=consId} 
        applyTrans ts1
        return lastRec
    
  polling= {- lock >> doit `finally` unlock where
   doit= -} do
      t<- emptyTrans  
      MyNode mynode<- atomically $ return . fromIDyn =<<  readTVar tvMyNode                                        
      
      case length $ nodesConected mynode of
        0 -> return t
        1 -> return t
        _  -> do
          handle(\e ->do{print e;return t}) $do
              addTrans t 
              return t


  transToSend   lastRec  = do
    [Just tvPendingTrans] <- getTVars [toIDyn  (PendingTrans undefined :: Data1 op a)]
    PendingTrans pendingTranss <- return . fromIDyn =<< readTVar tvPendingTrans

    let f t=getId t > lastRec 
    return .  sortBy compareIds $ filter  f  pendingTranss         -- `debug` ("transtosend PendingTrans= "++show pendingTranss)

  
  receive (senderNode,_,consolidated,ts) = do
    msg@(mnode, node', tvnode,lastId)  <- atomically $ do
      
      [mtvnode] <-  getTVars [toIDyn $ Node nod0{uri=senderNode}]   `debug` ("Receive: "++ show ts) 
      
      MyNode mynode <- return . fromIDyn =<< readTVar tvMyNode 
  
      tvnode<-  do
          case mtvnode of
            Nothing -> do 
                let node= Node nod0{uri=senderNode,lastReceived=id0
                                   ,consolidatedId=consolidated,active=Inactive}
  
                [tv] <- unsafeIOToSTM $ getTVarsIO [toIDyn node]   -- create the node in the cache
                tvnodes <- unsafeIOToSTM $ takeMVar tvNodes
                unsafeIOToSTM $ putMVar tvNodes $  tv:tvnodes
                return tv
  
            Just tv -> return tv                                      
            
      let URIHold myurl _= uri mynode 
      let myuri= uri mynode
                      
      node <-  do 
                Node nnode <-  return . fromIDyn =<< readTVar tvnode                                  
                let node= nnode{consolidatedId= consolidated}
                writeTVar tvnode $  toIDyn $ Node node 
                return node
  
      let prevLastSent= lastReceived node 
      let lastMyNode= lastReceived mynode                  `debug` ("Receiving from node=" ++ show node)
  
      let ts'= dropWhile (\t-> getId t <= lastMyNode) ts  --to avoid to apply already processed 
      
      let lastRecMyNode= if not $ null ts then getId $ last ts else lastMyNode
      
      let mynode'=  mynode{lastReceived= lastRecMyNode}
      writeTVar tvMyNode $ toIDyn $ MyNode mynode' 
  
      (node',lastId) <- case (active node, ts') of
  
        (Active, ts) -> do
                 lastId <- applyCSTrans ts
                 return (node,lastId)
      
        (Inactive,t@[Tr id nodes Connect]) -> do
                 let URIHold url _= senderNode
                 let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 writeTVar  tvMyNode $ toIDyn $ MyNode mynode''
                 lastId <- applyCSTrans t
                 return (node{active= ConnectReceived (length nodes) prevLastSent},lastId) `debug` ("new state= ConnectReceived")
               
        (Inactive, ts) -> do
                 lastId <- applyCSTrans . filter commutative $ ts'
                 return (node{active= ToSendConnect}   `debug` ("new state= ToSendConnect"),lastId)
  
        (ConnectSent,ts@[Tr _ _ (AddObjects os)]) -> do
                 
                 let URIHold url _= senderNode
                 let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 writeTVar  tvMyNode $ toIDyn $ MyNode mynode''
                 lastId <- applyCSTrans ts
                 return (node{active= Active},lastId)             `debug` ("new state= Active")
  
        (ConnectSent,ts) -> do
                 let URIHold url _= uri node
                 let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 writeTVar  tvMyNode $ toIDyn $ MyNode mynode''
  
                 lastId <- applyCSTrans ts
                 return (node{active= ToSendAddObjects prevLastSent},prevLastSent)  `debug` ("new state= ToSendAddObjects")
  
        x -> error $ "state not expected" ++ show x
  
      
      writeTVar tvnode $ toIDyn $ Node node'      --`debug` ("receive new state= "++ show (active node'))
      mnode<-  return . fromIDyn =<< readTVar tvMyNode   
      return (mnode, Node node', tvnode,lastId)
  
    return msg      --`debug` ("receive mynode="++show mnode)
             
    --receiveTrans :: (nodeId,Id,Id,[t])  -> IO (nodeId,Id,Id,[t])       
  respond (MyNode mynode, Node node, tvnode,lastRecFromNode)= atomically $ do
      let myuri= uri mynode
                   
      consid <- calcConsolidated node mynode lastRecFromNode   `debug` "Respond"

      tosend <- case active node of

         Active -> do   
               let lastRec = lastReceived node
               tsToSend<- transToSend $ lastRec
               
               return (myuri,lastRecFromNode , consid ,tsToSend) -- `debug` ("respond mynode="++show mynode)

      
         Inactive -> error $ "respond for a inactive node: "++show node         
          
         ConnectReceived  nodes prevLastSent -> do
            -- one side must drop its non-commutative transactions
              if length (nodesConected mynode) >= nodes 
               then do
                  --let prevLastSent= lastReceived node
                  tsOfReconnection <- reconnectAction  prevLastSent -- :: IO [Data1  op a]
                  id <- unsafeIOToSTM genIdNode                              `debug` "after reconnectAction"
                  let t= Tr id [] $ AddObjects  tsOfReconnection    
                  writeTVar tvnode $ toIDyn $ Node node{active=Active, consolidatedId=prevLastSent}   `debug` ("new state= Active")
                     --let URIHold url _= uri node

                  return (myuri,lastRecFromNode,consid ,[t])                    `debug`("antes de Respond return="  )
               else do
                  
                  return (myuri,lastRecFromNode,consid ,[ ])

         ToSendAddObjects prevLastSent-> do
                  tsOfReconnection <- reconnectAction  prevLastSent -- :: IO [Data1  op a]
                  
                  id <- unsafeIOToSTM genIdNode
                  let t= Tr id [] $ AddObjects  tsOfReconnection    
                  writeTVar tvnode $ toIDyn $ Node node{active=Active}          `debug` ("new state= Active")

                  return (myuri,lastRecFromNode,consid ,[t])

         ToSendConnect -> do  
                  id <- unsafeIOToSTM genIdNode
                  let conn = Tr id (nodesConected mynode) Connect 
                  writeTVar tvnode $ toIDyn $ Node node{active=ConnectSent}    `debug` ("new state= ConnectSent") 
  
                  return (myuri,lastRecFromNode,consid ,[conn])
      return tosend                                                            `debug` ("respond to send="++show tosend++ "to node="++ show (uri node))        
    where
    calcConsolidated node mynode lastRec=do
        tvnodes <- unsafeIOToSTM $ readMVar tvNodes
        nodes1 <- mapM  (\tv -> readTVar tv >>= return . fromIDyn :: STM ClusterData ) tvnodes 
        let nodes= deleteBy (\(Node n) (Node n')->uri n==uri n') (Node node) nodes1   `debug` ("tvnodes1="++show nodes1)
            
        let oldconsid= consolidatedId mynode
        
        return $ 
          case length nodes `debug` ("nodes="++show nodes ) of
              0 -> lastRec
              1 -> maximum [lastRec, consolidated] where 
                        consolidated=consolidatedId $ node
                        Node node= head nodes 
              _ -> let consIdsn=  --map (\(Node node) -> lastReceived node) nodes ++
                                  map (\(Node node) -> consolidatedId node) nodes 
                       consIds= delete id0 $ nub consIdsn

                   in  if null consIds then lastRec else minimum  consIds   `debug` ("consids="++show consIds)
     
  
  receiveTrans rq= {- lock >>  doit `finally` unlock   
   where
   doit= -}receive rq >>= respond    `debug` "receiveTrans" 
  
  sendTranss= {- lock >> doit `finally` unlock  
   where  
   doit= -} do
     MyNode mynode <-atomically $ return . fromIDyn =<<  readTVar tvMyNode             
     tvnodes <- readMVar tvNodes --getTVars [getProto n | n <- nodesConected mynode]  :: IO [(TVar(Data1 op a))]
     
     handle(\e ->do {print e; return (uri mynode)}) $ do
       flags <- mapM  (send1 $ MyNode mynode)  tvnodes 
       if foldr  (||) False flags then return ()  -- if all are inactive
           else  atomically (applyCSTrans  []) >> return ()  --to process local transactions even if no received transactions or no cluster
       return (uri  mynode) `debug` "end sendTrans"
  
       where
       send1 :: ClusterData -> TVar IDynamic -> IO Bool
       send1 (MyNode mynode) tvnode = do
          Node node <- atomically $ return . fromIDyn =<<  readTVar tvnode
          if  active node /= Active  then return False else do
            resp<-   respond (MyNode mynode,  Node node, tvnode,lastReceived mynode) :: IO (URIHold,Id,Id,[Tr (Op op a) ] )            
            case resp of
              (_,_,_,[]) -> return True                      `debug`   "nada que enviar"
              _  -> do 
                       sendT resp   tvnode   (Node node)
                       return True 
          where
           sendT :: (URIHold,Id,Id,[Tr (Op op a) ] ) -> TVar IDynamic ->ClusterData -> IO ()
           sendT resp  tvnode (Node node) = do
             mts <- send resp (uri node)                   `debug` ("sendTrans to node="++ show node)
             case mts of
                Just msg@(_,lastRec,_,_)  -> do
                           receive msg
                           atomically $ writeTVar tvnode $ toIDyn $ Node node{lastReceived= lastRec}
                           
                           return()
                               
              
                Nothing -> do
                   atomically $ do
                      writeTVar tvnode $ toIDyn $ Node node{ active= Inactive}
                      let URIHold url _ = uri node
                      writeTVar tvMyNode $  toIDyn $ MyNode mynode{nodesConected= nodesConected mynode \\ [url]} 
                   return ()
  

addTransaction :: Cluster  (Tr (Op op a)) (Data1 op a) URIHold Id =>[String] -> op  -> IO ()
addTransaction keys op  =  createTrans keys op >>= addTrans 

createTrans :: Cluster  (Tr (Op op a)) (Data1 op a) URIHold Id => [String] ->  op  -> IO (Tr (Op op a))
createTrans keys op = do
    id <- genIdNode                              
    return $ Tr id keys  (Op op)

      

compareIds t t'= compare (getId t) (getId t')




      
loop interval f=loop1 
        where 
        loop1 =  do 
                 f
                 threadDelay (fromIntegral $ interval * 1000000)                    `debug` "loop"
                 loop1  

lockReceive= unsafePerformIO $ newMVar True
lock= takeMVar lockReceive   -- to queye all requests of all nodes
unlock= putMVar lockReceive True


tvMyNode :: TVar IDynamic
tvMyNode= tv where [tv]= unsafePerformIO $ getTVarsIO [toIDyn $ MyNode nod0 `debug` "initializing MyNode" :: IDynamic]

tvAllObjects :: TVar IDynamic
tvAllObjects =  tv where [tv]= unsafePerformIO $ getTVarsIO [toIDyn $ AllObjects 0 [] :: IDynamic]

--tvPendingTrans :: TVar IDynamic
--tvPendingTrans= tv where [tv]= unsafePerformIO $ getTVarsIO [toIDyn $ PendingTrans [] `debug` "initializing PendingTrans" ]   

tvNodes :: MVar [TVar IDynamic]
tvNodes = unsafePerformIO $ do
        tvnodes<- atomically $ do 
          MyNode mynode <-    return . fromIDyn =<< readTVar tvMyNode             
          let nodeIds = nodesConected mynode
          mtvnodes <- getTVars [toIDyn $ Node nod0{uri=URIHold n undefined}| n <- nodeIds]
          return (catMaybes mtvnodes)                                 `debug` "initializing tvNodes"
          
        newMVar  tvnodes
