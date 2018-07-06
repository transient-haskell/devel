{-# OPTIONS  -fglasgow-exts -XUndecidableInstances  -XOverlappingInstances #-}
module ClusterTCache where
import Transaction
import Cluster
import URIHold
import Protocol
import ProtocolHTTP
import Data.TCache.Dynamic
import Data.Typeable
import Data.Maybe(isJust,fromJust,mapMaybe, catMaybes)
import Control.Exception(handle, finally, SomeException)
import Data.List (sortBy,nubBy,dropWhile,deleteBy)
import Data.HashTable(hashString)

import System.IO.Unsafe
import Control.Concurrent.STM(STM,atomically,retry)
import Control.Concurrent.STM.TVar --hiding (takeTMVar,putTMVar)
import GHC.Conc	(unsafeIOToSTM)
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map as M
import Data.List(partition,nub,find,(\\),delete,isPrefixOf)
import System.Time
import Control.Exception(assert)
import Control.Monad(when, liftM)
import Data.Monoid

import Debug.Trace

{-
takeTMVar :: IResource a => TMVar a -> STM a
takeTMVar  t = do
  m <- tryTakeTMVar t --readTVar t
  case m of
    Nothing -> retry        `debug` "takeTMVar: Nothing"
    Just a  -> return a     `debug` ("takeTMVar "++ keyResource a)

putTMVar :: IResource a =>  TMVar a -> a -> STM ()
putTMVar  t a = do
  m <- tryPutTMVar t a --readTVar t
  case m of
    True ->  return ()    `debug` ("putTMVar "++ keyResource a)
    False  -> retry       `debug` ("putTMVar Blocked for "++ keyResource a)

-}

readTMVar= readTVar
takeTMVar= readTVar
putTMVar= writeTVar
swapTMVar= writeTVar

getTMVarsIO= getTVarsIO
getTMVars= getTVars


cleanTimes= 100  -- every 100 transactions the AllObjects list in memory will remove duplicate keys


data Data1 op a = PendingTrans [Tr op a]
                | Key1 String
                | Data1 a
                deriving (Read, Show, Eq,Typeable)


data Op op a=  AddObjects [Data1 op a] | Connect Int | Noop  Int |  Op op  deriving (Read, Show, Eq)

data ObjRep a= KeyObj String | Obj (Maybe a) deriving (Read,Show)

type DoReify= Bool
type Source= Int

data Tr op a= --  Tr Source Id DoReify [ObjRep (Data1 op a)] (Op op a) deriving (Read, Show)
       Tr {source :: Int,  tid :: Id, doReify :: Bool,  objRep :: [ObjRep (Data1 op a)] , objs :: Op op a}
       deriving (Read, Show)

instance Eq (Tr op a) where
  Tr id _ _ _ _== Tr id' _ _ _ _= id== id'

datdir= "data/"

instance (Show op, Show a, Read op, Read a,IResource a) => IResource (Data1 op a )  where
   keyResource (Data1 a)        = keyResource a
   keyResource (PendingTrans _) = "PendingTrans"
   keyResource (Key1 s)= s
   serialize x    = show x
   deserialize x = read x
   defPath _ = datdir++show s++"/" where
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
class (Eq op, Eq a)=> TransMap op a | op -> a, a -> op  where
  mapping :: op -> ([Maybe a]-> [a])
  commutativeOp :: op -> Bool
  commutativeOp op= True

instance TransMap op a => TransMap  (Op op a) (Data1 op a)  where

  mapping (Connect _)  = \_-> [  ]
  mapping (Noop _) = \_ -> []

  mapping (AddObjects as) = \_ ->   as                                 `debug` "applying AddObjects"


  mapping (Op op) = f $ mapping op where
          f f1 dmas =  map (\a-> Data1 a) as where
                 as= f1 mas
                 mas= map filter dmas
                 filter (Just (Data1 a))= Just a
                 filter Nothing = Nothing

instance  TransMap  op a => Trans (Tr op a) (Data1 op a) Id where
  --apply:: t -> [Maybe a] -> IO [a]
  apply (Tr _ _ _ _ op) mas = (mapping op) mas                                                                        `debug` "apply"

  --getId:: t -> id
  getId  tr= tid tr


  commutative (Tr _ _ _ _ (Connect _ )) = False
  commutative (Tr _ _ _ _ (Noop _)) = True
  commutative (Tr _ _ _ _ (AddObjects _ )) = False

  commutative (Tr _ _ _ _ (Op op)) = commutativeOp op

  nullTrans= undefined
  allTransactions=undefined
  applyTo _= undefined


instance TransMap op a => NTrans (Tr op a) (Data1 op a)  where
  --getNode :: t -> Int
  getNode t= node where Id _ _  node= getId t



instance (Show op, Show a, IResource op, TransMap (Op op a) (Data1 op a)
          ,IResource  (Data1 op a)
          ,Trans (Tr op a) (Data1 op a) Id
          ,Typeable op, Typeable a)
          => SyncRepository (Tr op a) (Data1 op a) Id
   where



   addTrans t= trace "1" atomically $ do
            withSTMResources[PendingTrans undefined]
              (\[Just (PendingTrans ts)]-> resources {toAdd= [PendingTrans (t:ts)]})
            --[Just tvPendingTrans] <- getTMVars [toIDyn (PendingTrans undefined :: Data1 op a)]
            --PendingTrans ts <- return . fromIDyn =<< takeTMVar tvPendingTrans
            --putTMVar tvPendingTrans $ toIDyn $ PendingTrans (t:ts)

   emptyTrans= do
            id <- unsafeIOToSTM $ genIdNode                                            -- `debug` "emptyTrans"
            MyNode mynode<-   return . fromIDyn =<< readTMVar tvMyNode               `debugf` " emptyTrans mynode"
            return $  Tr 0 id False []  . Noop . length $ nodesConected mynode



   reify (Tr s id _ objs op) =
     do
        let getResource1 s=  withSTMResources [Key1 s :: (Data1 op a)]
               (\[r]-> (resources{toReturn=  r} :: Resources (Data1 op a)(Maybe(Data1 op a))))

        MyNode mynode<-  return . fromIDyn =<< readTMVar tvMyNode
        let f = isPrefixOf (prefix mynode)

        let reify1 (Obj mo) = return $ Obj mo
            reify1 (KeyObj s)=
               if f s then liftM Obj $ getResource1 s else return $ KeyObj s
            {-
                   mr <-  getResource1 s
                   case mr of
                     Nothing -> return $ Obj Nothing   :: STM (ObjRep (Data1 op a))
                     Just r  -> return $ Obj $ Just r
               else return $ KeyObj s
            -}
        objs'<- mapM reify1 objs
        let iskey (KeyObj s)= True
            iskey  _ = False
        if not  .  null $ filter  iskey objs' then return $ Tr s id True  objs' op
                                                     else return $ Tr s id False objs' op

   applyTrans ts= do
        MyNode mynode<-  return . fromIDyn =<< readTMVar tvMyNode
        let f= isPrefixOf (prefix mynode)
        ts' <- apply0 f  ts
        return ts'             `debug` ("applyTrans return:"++ show (ts' :: [Tr op a]))

        where
        reifyOnDemand t@(Tr _ id False objs op)= return t :: STM (Tr op a)
        reifyOnDemand t= reify t

        apply0 :: (String -> Bool) -> [Tr op a] ->  STM [Tr op a]
        apply0 _  []= return []
        apply0 f (t:ts)= do
           mr<- apply1 f t
           case mr of
             Just r -> do
                        rs <- mapM reifyOnDemand ts :: STM [Tr op a]
                        return (r:rs)
             Nothing -> apply0 f ts


        apply1 :: (String -> Bool) -> Tr op a -> STM (Maybe (Tr op a))
        apply1 f t@(Tr _ d reify1 objs op)= case reify1  of
             True -> do t' <- reify t :: STM (Tr op a)
                        return $ Just t
             False-> do
                  t@(Tr s id reify1 objs' op) <- reify t  :: STM (Tr op a)

                  if reify1 then return $ Just t
                       else
                       do
                       let(id,(outobjs, keys1)) =apply2 t $ map (\(Obj x)-> x) objs'

                       withSTMResources ([]:: [Data1 op a]) (\_-> resources{toAdd= outobjs } )    `debug` ("applyTrans outobjs keys="++show keys1)


                       AllObjects n' xs' <- return . fromIDyn =<< takeTMVar tvAllObjects :: STM ClusterData

                       --TOD t _ <-  unsafeIOToSTM getClockTime
                       let keys= zip keys1 (repeat id)
                       let (n, xs) =let l= keys++xs'; nn= n'+ length keys1
                                    in if n' `mod` cleanTimes ==0 then  (nn,nubBy nubBy1 l)
                                                                  else (nn,l)
                       putTMVar tvAllObjects $ toIDyn $ AllObjects n xs

                       return Nothing

        nubBy1 (s,_)(s',_)= s==s'


        apply2 t objs=
          let
           outobjs= apply t objs               `debug`("applyTrans objs=" ++ concatMap serialize ( catMaybes objs))
           keys1= map keyResource outobjs  where

          in (getId t, (outobjs, keys1) )    `debug` ("**applyTrans*** "++concatMap serialize outobjs)






instance IResource ClusterData where
   keyResource (Node Nod{uri=URIHold s _})    = show $ hashString s
   keyResource (MyNode _)       = "MyNode"
   keyResource (AllObjects _ _ )= "AllObjects"

   serialize x   = show x
   deserialize x = read x
   defPath _ = datdir++show s++"/" where
                           url= unsafePerformIO $ readMVar myNodeName
                           s  = uriPort1 $ makeURI url


type Message op a= (URIHold,Id,Id,[Tr op a])


instance  (Show a, Read a, Show op, Read op
          ,Protocol (Message op a) URIHold
          ,SyncRepository (Tr op a) (Data1 op a) Id

          ,IResource (Data1 op a)
          ,Typeable (Data1 op a)
          ,TransMap op a
          ,Typeable a, Typeable op

          ,NTrans (Tr op a) (Data1 op a) )
          => Cluster (Tr op a) (Data1 op a) URIHold Id where


  reconnectAction id = do
        AllObjects n all <-  return . fromIDyn  =<< readTMVar tvAllObjects      --  `debug` ( "reconnect id="++ show id)    :: STM ClusterData
        let (all1,ms)= unzip . filter (\(_,mod)->  mod> id) $ nubBy nubBy1 all

           --let t= unsafeCoerce 1
        objs <-  withSTMResources (map Key1 ( all1 \\ ["PendingTrans","MyNode"]) ) f      `debug`  ("all="++ show all++" all1="++  show all1)
        return (maximum ms, objs)

        where
        f mas= resources {toReturn=catMaybes mas } `debug`  (" allResources="++ show mas) ::Resources (Data1 op a) [(Data1 op a)]
        nubBy1 (s,_)(s',_)= s==s'





  initCluster myurl mclusterurl interval filter= do

        registerType :: IO ClusterData
        registerType :: IO (Data1 op a)
        tvnodes<- readMVar tvNodes
        withResources ([] :: [Data1 op a]) (\_-> [PendingTrans [] :: Data1 op a])

        clearSyncCacheProc  10 defaultCheck 10  `debug` "initCluster"
        let myuri= makeURI myurl
        let port= uriPort1 myuri
        modifyMVar_   myNodeName (\_->  return myurl)


        forkIO $  setReceiver port (receiveTrans :: Message op a  -> IO (Message op a))
        threadDelay 1000000
        forkIO $ loop interval (sendTranss  interval :: IO URIHold)
        threadDelay 1000000
        --forkIO $ loop interval (polling :: IO (Tr op a))

        when (isJust mclusterurl) $ do
          let clusteruri= makeURI . fromJust $ mclusterurl

          [tv] <- trace "2" atomically $ getTMVars[toIDyn $ Node nod0{uri=clusteruri}]

          case tv of
              Nothing -> connect myuri clusteruri

              _ -> return ()

        trace "3" atomically $ do
                           MyNode nod <- return . fromIDyn =<< takeTMVar  tvMyNode
                           putTMVar tvMyNode $ toIDyn $ MyNode nod{uri= myuri, prefix=filter}


        return myuri

        where
        connect myuri clusteruri=do
          MyNode mynode <- atomically $  return  .  fromIDyn =<<   readTMVar tvMyNode
          let source=  length( nodesConected mynode) + 1
          [tv] <- getTMVarsIO[toIDyn $ Node nod0{number=source, uri=clusteruri, active=ConnectSent}]     --create the other computer node
          modifyMVar_ tvNodes(\tvnodes -> return $ tv:tvnodes)


          id <- genIdNode
          mts <- send (myuri, id0, id0,[Tr 0 id False  []{- XXXX-} (Connect 0):: Tr op a])  clusteruri --send a request to a node of the cluster


          case mts `debug`  ( "connection response:"++ show mts) of
              Just msg  -> do
                         print "Connect sent"
                         trace "4" atomically $ receive msg
                         return()


              Nothing -> do
                 error $ "cluster at adddress "++show clusteruri++" not reached"



          threadDelay 1000000

          trace "5" atomically $ do   -- wait until the node has been connected
              MyNode nod <- return . fromIDyn =<< readTMVar tvMyNode
              if not $ show clusteruri `elem` nodesConected nod
                then retry                                             `debug` ("not conected, nodes="++ show( nodesConected nod))
                else return()
          return ()

  queueTrans  [] = do
          MyNode mynode <-  return . fromIDyn =<<  readTMVar tvMyNode
          return $ lastReceived mynode

  queueTrans ts= do
        MyNode mynode <-  return . fromIDyn =<<  readTMVar tvMyNode
        [Just tvPendingTrans] <- getTMVars [toIDyn (PendingTrans undefined :: Data1 op a)]
        PendingTrans all <- return . fromIDyn =<< takeTMVar tvPendingTrans  :: STM (Data1 op a)
        putTMVar tvPendingTrans $ toIDyn $ PendingTrans $  all ++ ts
        let
               firstValid []=  lastReceived mynode
               firstValid (t:ts)= let id= getId t in if id /=id0 then id else firstValid ts

        return $ firstValid . reverse $ ts

  applyCSTrans  = do

        MyNode mynode <-  return . fromIDyn =<<  readTMVar tvMyNode
        let nodeIds = nodesConected mynode

        --Just tvtrans:tvnodes <-  unsafeIOToSTM $ getTMVars $ PendingTrans u:[Node nod0{uri=URIHold n u}| n <- nodeIds]
        [Just tvPendingTrans] <- getTMVars [toIDyn (PendingTrans undefined :: Data1 op a)]
        PendingTrans all <- return . fromIDyn =<< takeTMVar tvPendingTrans  :: STM (Data1 op a)
        tvnodes <- unsafeIOToSTM $  readMVar tvNodes                       `debug` ("applyCSTrans PendingTrans init: "++ show all)
        let f tv= readTMVar tv >>=   return  .  fromIDyn
        nodes <-  mapM f  tvnodes

        let eqID x y= getId x==getId y
        let tss =  nubBy eqID $ sortBy compareIds all

        let
               firstValid []= lastReceived mynode
               firstValid (t:ts)= let id= getId t in if id /=id0 then id else firstValid ts
        let lastRec= firstValid . reverse $ tss
        --let lastRec=  if not $ null ts then getId $ last ts else lastReceived mynode
        let oldconsid= consolidatedId mynode


        let consId' =
                     let
                       consIdsn=   map (\(Node node) -> consolidatedId node) nodes
                       lastRecs= mapMaybe (\ (Node node)  -> if isFinalNode node then Just  $ lastReceived node else Nothing) nodes
                       consIds= {-delete id0 $ -}nub $ consIdsn  ++  lastRecs

                     in  if null consIds then lastRec else minimum  consIds                  `debug` ("NODES="++show nodes)

        let (ts1',ts2) =  if null $ nodesConected mynode
                         then (tss,[])
                                else span (\t-> getId t<= consId') tss


        ts1 <- applyTrans ts1'
        let consId= if null ts1 then consId' else getId $ head ts1'                                `debug` ("consId= "++show consId)

        ts3<- case (ts1++ts2,(oldconsid /= consId)) of
              ([],True) -> do
                      t <- emptyTrans
                      return [t]                 -- to forward the new consId to other nodes
              (ts,_) -> return ts


        putTMVar tvPendingTrans $ toIDyn $ PendingTrans ts3                          `debug` ("applyTrans final PendingTrans: "++ show ts3)
        swapTMVar tvMyNode $ toIDyn $ MyNode mynode{consolidatedId=consId}

        return lastRec
{-
  polling= do
    --lock
    r <- doit `debug` "vvvvvvvvvv polling vvvvvvvvvvv"
    --unlock `debug` "*******end polling******"
    return r
   where
   doit=  do
      t<- atomically $ emptyTrans
      handle(\ (e :: SomeException)  -> do{print e;return t}) $do
              addTrans t
              return t
-}

  transToSend src   lastRec  = do
    [Just tvPendingTrans] <- getTMVars [toIDyn  (PendingTrans undefined :: Data1 op a)]
    PendingTrans pendingTranss <- return . fromIDyn =<< readTMVar tvPendingTrans

    -- select the  transactions that did not come from that node, that are new and that Noop operations from myself not generated directly by this node.
    let f t =  source t /= src  &&  getId t > lastRec  &&  not (isNoop t && source t /= 0) where isNoop t= case objs t of  Noop _ -> True;   _ -> False
    let tosend =  sortBy compareIds $ filter  f  pendingTranss         `debug` ("transtosend PendingTrans= "++show pendingTranss)
    tosend2 <- if null tosend then emptyTrans  >>= \t -> return [t] else return tosend
    return $ tosend2    `debug` ("transtosend = "++show tosend2)

  receive (senderNode,lastRec,consolidated,ts) = do
    msg@(mnode, node', tvnode,lastId)  <- do

      [mtvnode] <-  getTMVars [toIDyn $ Node nod0{uri=senderNode}]   `debug` ("Receive: "++ show ts)

      MyNode mynode <- return . fromIDyn =<< takeTMVar tvMyNode

      tvnode <-  do
          case mtvnode of
            Nothing -> do
                let source= length (nodesConected mynode) + 1
                let node= Node nod0{ uri=senderNode, number=source, lastReceived=lastRec
                                                , consolidatedId=consolidated, active=Inactive}    `debug` ("source="++ show source)

                [tv] <- unsafeIOToSTM $ getTMVarsIO [toIDyn node]   -- create the node in the cache
                unsafeIOToSTM $ modifyMVar_ tvNodes(\tvnodes-> return $ tv:tvnodes)
                return tv

            Just tv -> return tv

      let URIHold myurl _= uri mynode
      let myuri= uri mynode

      let lastMyNode= lastReceived mynode

      (prevLastSent,node) <-  do
                Node nnode <-  return . fromIDyn =<< takeTMVar tvnode
                let node= nnode{lastReceived=lastRec,consolidatedId= consolidated}
                putTMVar tvnode $  toIDyn $ Node node
                return (lastReceived nnode, node)           `debug` ("lastReceived=" ++ show lastMyNode)


      let consid= consolidatedId mynode
      let ts'= map (\t -> t{source=number node}) . dropWhile (\t-> getId t <= consid)  $  sortBy compareIds ts  --to avoid to apply already processed

      let lastRecMyNode= if not $ null ts then getId $ last ts else lastMyNode

      let mynode'=  mynode{lastReceived= lastRecMyNode}
      putTMVar tvMyNode $ toIDyn $ MyNode mynode'

      (node',lastId) <- case (active node, ts')  `debug` ("active=" ++ show ( active node)) of



        (Active, t@[Tr _ id _ [] (Noop n)]) -> do
                let node'= node{isFinalNode= if n <= 1 then True  else False}
                writeTVar tvnode . toIDyn  $ Node node'                                          `debug` "set isFinalNode"
                lastId <- queueTrans t
                return (node',  lastId)

        (Active, ts) -> do
                 lastId <- queueTrans ts
                 return (node,lastId)

        (Inactive,t@[Tr _ id _ [] (Connect n)]) -> do
                 --let URIHold url _= senderNode
                 --let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 --swapTMVar  tvMyNode $ toIDyn $ MyNode mynode''

                 --lastId <- queueTrans t
                 return (node{active= ConnectReceived n prevLastSent, isFinalNode= if n==0 then True else False }, lastRecMyNode) --`debug` ("new state= ConnectReceived")

        (Inactive, ts) -> do
                 lastId <- queueTrans . filter commutative $ ts'
                 return (node{active= ToSendConnect} ,lastId)                 -- `debug` ("new state= ToSendConnect")

        (ConnectSent,ts@[Tr _ _ _ _ (AddObjects _)]) -> do

                 let URIHold url _= senderNode
                 let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 swapTMVar  tvMyNode $ toIDyn $ MyNode mynode''
                 lastId <- queueTrans ts
                 return (node{active= Active},lastId)                             --`debug` ("new state= Active")

        (ConnectSent,ts) -> do
                 let URIHold url _= uri node
                 let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 swapTMVar  tvMyNode $ toIDyn $ MyNode mynode''

                 lastId <- queueTrans ts
                 return (node{active= ToSendAddObjects prevLastSent},prevLastSent)  --`debug` ("new state= ToSendAddObjects")

        (AddObjectsSent,ts) -> do

                 let URIHold url _= senderNode
                 let mynode''= mynode'{nodesConected= url:nodesConected mynode}
                 swapTMVar  tvMyNode $ toIDyn $ MyNode mynode''
                 lastId <- queueTrans ts
                 return (node{active= Active},lastId)                             --`debug` ("new state= Active")


        x -> error $ "state not expected" ++ show x


      swapTMVar tvnode $ toIDyn $ Node node'                                       `debug` ("receive new state= "++ show (active node'))
      mnode<-  return . fromIDyn =<< readTMVar tvMyNode
      return (mnode, Node node', tvnode,lastId)

    return msg                                                                    --`debug` ("receive mynode="++show mnode)

    --receiveTrans :: (nodeId,Id,Id,[t])  -> IO (nodeId,Id,Id,[t])
  respond (MyNode mynode, Node node, tvnode,lastRecFromNode)=  do
      let myuri= uri mynode

      consid <- calcConsolidated node mynode lastRecFromNode   `debug` "Respond"

      tosend <- case active node of

         Active -> do
               --let lastRec = lastReceived node
               tsToSend <- transToSend (number node) $ consolidatedId mynode  -- lastRec

               return (myuri,lastRecFromNode , consid ,tsToSend)           -- `debug` ("respond mynode="++show mynode)


         Inactive -> error $ "respond for a inactive node: "++show node

         ConnectReceived  nodes prevLastSent -> do
            -- one side must drop its non-commutative transactions
              if length (nodesConected mynode) >= nodes
               then do
                  (id, tsOfReconnection) <- reconnectAction  prevLastSent  -- :: STM (Id, [Data1  op a])
                  --id <- unsafeIOToSTM genIdNode                              `debug` "after reconnectAction"
                  let t= Tr 0 id  False [] $ AddObjects  tsOfReconnection
                  swapTMVar tvnode $ toIDyn $ Node node{active=AddObjectsSent, consolidatedId=prevLastSent}   `debug` ("new state= AddObjectsSent")
                     --let URIHold url _= uri node

                  return (myuri,lastRecFromNode,consid ,[t])                    `debug`("antes de Respond return="  )
               else do

                  return (myuri,lastRecFromNode,consid ,[ ])


         ToSendAddObjects prevLastSent-> do
                  (id, tsOfReconnection) <- reconnectAction  prevLastSent -- :: IO [Data1  op a]

                  --id <- unsafeIOToSTM genIdNode
                  let t= Tr 0 id False [] $ AddObjects  tsOfReconnection
                  swapTMVar tvnode $ toIDyn $ Node node{active=AddObjectsSent}          `debug` ("new state= AddObjectsSent")

                  return (myuri,lastRecFromNode,consid ,[t])

         ToSendConnect -> do
                  id <- unsafeIOToSTM genIdNode
                  let conn = Tr 0 id False  [] (Connect (length $ nodesConected mynode))
                  swapTMVar tvnode $ toIDyn $ Node node{active=ConnectSent}    `debug` ("new state= ConnectSent")

                  return (myuri,lastRecFromNode,consid ,[conn])
      return tosend                                                            `debug` ("respond to send="++show tosend++ "to node="++ show (uri node))
    where
    calcConsolidated node mynode lastRec=do
        tvnodes <- unsafeIOToSTM $ readMVar tvNodes
        nodes1 <- mapM  (\tv -> readTMVar tv >>= return . fromIDyn :: STM ClusterData ) tvnodes
        let nodes= deleteBy (\(Node n) (Node n')-> uri n==uri n') (Node node) nodes1   -- `debug` ("tvnodes1="++show nodes1)

        let consIdsn=   map (\(Node node) -> consolidatedId node) nodes
        let lastRecs= mapMaybe (\(Node node) -> if isFinalNode node then Just  $ lastReceived node else Nothing) nodes
        let consIds= delete id0 $ nub consIdsn  ++  lastRecs

        return $ trace ("calcConsolidated: lastRec="++show lastRec ++ "node=" ++ show node ++ " nodes="++ show nodes ++" mynode="++ show mynode ) $
                  if null consIds then lastRec else minimum  consIds                  `debug` ("NODES="++show nodes)

        `debugf` "calcConsolidated"






  receiveTrans rq= atomically $ receive rq  >>= respond


  sendTranss interval=  do
     atomically $ applyCSTrans
     (mynode,messages)<- trace "8" atomically $ do
        MyNode mynode <-  return . fromIDyn =<<  readTMVar tvMyNode
        messages <- toSend mynode :: STM [(Message op a , TMVar IDynamic ,Nod)]
        return (mynode, messages)

     when (not $ null messages ) $  mapM_ (send1 mynode) messages

     return $ makeURI ""
     where
     toSend :: Nod -> STM [(Message op a , TMVar IDynamic ,Nod)]
     toSend mynode= do
       tvnodes <- unsafeIOToSTM $ readMVar tvNodes               --getTVars [getProto n | n <- nodesConected mynode]  :: IO [(TVar(Data1 op a))]

       mmessages <- mapM  (toSend1  mynode)  tvnodes
       let messages = catMaybes mmessages
       return messages
       where
       toSend1 :: Nod -> TMVar IDynamic -> STM (Maybe(Message op a  , TMVar IDynamic ,Nod))
       toSend1  mynode tvnode = do
          Node node <- return . fromIDyn =<<  readTMVar tvnode
          if  okToSend node
           then  do
            resp<-   respond (MyNode mynode,  Node node, tvnode, lastReceived mynode )
            case resp of
              (_,_,_,[]) -> return Nothing                      `debug`   "nothing to send"
              msg -> return $ Just (msg,tvnode ,  node)
           else return Nothing
          where
          okToSend node=
                     let
                         id@Id {time= TTOD last _}= lastReceived node
                         tnow x= unsafePerformIO getClockTime
                         TOD tnow1 _=tnow 1
                         ret=  active node == Active && tnow1 - last > fromIntegral interval  -- interval from last send
                     in  ret `debug` ("okToSend=" ++ show ret++" active="++ show (active node) ++" last="++show last++" tnow="++show tnow1)

     send1 :: Nod -> (Message op a, TMVar IDynamic , Nod) -> IO ()
     send1  mynode (resp,  tvnode,  node) = do
             mts <- send resp (uri node)                        `debug` ("sendTrans to node="++ show (uri node))
             case mts of
                Just msg@(_,lastRec,_,_)  -> do
                           trace "9" atomically $ do
                                           receive msg
                                           --swapTMVar tvnode $ toIDyn $ Node node{lastReceived= lastRec}
                                           -- XXX poner esto en receive no?
                           return ()


                Nothing -> do
                   trace "10" atomically $ do
                      swapTMVar tvnode $ toIDyn $ Node node{ active= Inactive}
                      let URIHold url _ = uri node
                      swapTMVar tvMyNode $  toIDyn $ MyNode mynode{nodesConected= nodesConected mynode \\ [url]}
                   return ()


addTransaction :: Cluster  (Tr op a) (Data1 op a) URIHold Id =>[String] -> op  -> IO ()
addTransaction keys op  =  createTrans keys op >>= addTrans

createTrans :: Cluster  (Tr op a) (Data1 op a) URIHold Id => [String] ->  op  -> IO (Tr op a)
createTrans keys op = do
    id <- genIdNode
    return $ Tr 0 id False (map KeyObj keys)  (Op op)



compareIds t t'= compare (getId t) (getId t')





loop interval f=loop1
        where
        loop1 =  do
                 threadDelay (fromIntegral $ interval * 1000000)                    `debug` "loop"
                 f
                 loop1

lockReceive= unsafePerformIO $ newMVar True
lock= takeMVar lockReceive   -- to queye all requests of all nodes
unlock= putMVar lockReceive True


tvMyNode :: TMVar IDynamic
tvMyNode= tv where [tv]= unsafePerformIO $ getTMVarsIO [toIDyn $ MyNode nod0 `debug` "initializing MyNode" :: IDynamic]

tvAllObjects :: TMVar IDynamic
tvAllObjects =  tv where [tv]= unsafePerformIO $ getTMVarsIO [toIDyn $ AllObjects 0 [] :: IDynamic]

--tvPendingTrans :: TMVar IDynamic
--tvPendingTrans= tv where [tv]= unsafePerformIO $ getTMVarsIO [toIDyn $ PendingTrans [] `debug` "initializing PendingTrans" ]

tvNodes :: MVar [TMVar IDynamic]
tvNodes = unsafePerformIO $ do
        tvnodes<- trace "11" atomically $ do
          MyNode mynode <-    return . fromIDyn =<< readTMVar tvMyNode
          let nodeIds = nodesConected mynode
          mtvnodes <- getTMVars [toIDyn $ Node nod0{uri=URIHold n undefined}| n <- nodeIds]
          return (catMaybes mtvnodes)                                 `debug` "initializing tvNodes"

        newMVar  tvnodes
