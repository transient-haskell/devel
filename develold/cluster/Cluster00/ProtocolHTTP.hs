{-# OPTIONS  -fglasgow-exts -fallow-undecidable-instances #-}
module ProtocolHTTP where
import Protocol
import Network
import Network.HTTP
import Network.TCP
import Network.URI
import Network.Socket
import System.Process
import System.IO
import System.IO.Unsafe
import Control.Concurrent
import Control.Exception(catch, finally)
import qualified Data.Map as M
import Control.Concurrent.MVar
import Data.IORef
import Data.List(elemIndex)
import URIHold
import Control.Exception(handle)
import Control.Monad(when)
import Cluster.Debug(forkIOd)

debug a b= a

data Connection1 = Conn Connection | Sock Socket

sockets= unsafePerformIO $ newMVar $ (M.empty :: M.Map String (Connection1))


makeURI url= URIHold url uri 
    where 
    uri= case parseURI url of
            Nothing -> error $ "syntax error on url:"++ show uri
            Just uri-> uri

uriPort1 (URIHold url uri)= case uriAuthority uri of
        Nothing -> error $ "no urlAuthority in "++ show uri  
        Just auth -> case tail $ uriPort auth of
                       "" -> 80
                       s  -> read s

instance (Read t, Show t) => Protocol  t URIHold where
   send x (URIHold surl url)= handle (\e-> return Nothing) $ do 
      msockets <- readMVar sockets                                                  --`debug` ("url="++ show url)
      let mauth= uriAuthority url
      case mauth of
         Nothing -> error $ "URI: bad syntax: " ++ surl
         Just auth -> 
          case M.lookup surl msockets of
           Nothing   ->  do               
                connection <- newConn
                send1 connection x url          
           Just (Sock sock) -> do 
                open <- sIsConnected sock
                if open then send1 sock x url 
                        else  do modifyMVar_ sockets (\s->return $ M.delete surl s)
                                 connection <- newConn      `debug` "connection was closed"
                                 send1 connection x url   
                                 
           Just (Conn connection) -> send1 connection x url
      where
      
      newConn=  do
                connection <- openTCPPort url1  port                                `debug` ("send: conectando con "++show url1)
                modifyMVar_ sockets( \msockets-> return $ M.insert surl  (Conn connection) msockets ) 
                                                                                    `debug` "despues de conectar"
                return connection 

      (url1,port)= case uriAuthority url of
        Nothing -> error $ "no urlAuthority in "++surl  
        Just auth ->  split ':' $ uriUserInfo auth ++ uriRegName auth ++ uriPort auth
       
      split c str=case elemIndex c str  of
         Nothing    -> (str,80)
         Just index -> (s, read r) where (s,':':r)= splitAt index str

      send1 sock x url=   do
           putStrLn $ "send: " ++ show x
           rresp <- sendHTTP  sock $! genReq x url                               `debug` ("to send***"++show x)
           case rresp `debug` ("PROCESSING RESP: respBody="++ let Right resp=rresp in rspBody resp) of
              Left str     -> do
                               print str                                          `debug` "****response error*****"
                               return Nothing
              Right resp ->     do
                   putStrLn $ "resp=" ++ show resp

                   if take 2 (rspReason resp) =="OK" then do
                     
                                let ts' = read $ rspBody resp                          `debug`("respBody="++ rspBody resp)
                                print ""                                               `debug` "****response received***"
                                return $ Just ts'                                      
                        else return Nothing                                            `debug` ( "**** \""++rspReason resp++"\" received ****")
           where
           genReq os comp= 
             Request { rqURI = comp
                     , rqMethod= POST
                     , rqHeaders= [Header HdrContentLength $ show $ length str
                                  ,Header HdrUserAgent "Haskell clustering"
                                  --,Header HdrTransferEncoding "chunked"
                                  ,Header HdrContentType "text/plain"
                                  ,Header HdrAccept "text/plain"
                                  ,Header HdrAcceptEncoding "gzip,deflate"
                                  ,Header HdrAcceptCharset  "ISO-8859-1,utf-8;q=0.7,*;q=0.7"
                                  ,Header HdrAcceptLanguage "en-us,en;q=0.5"

                                  --,Header HdrConnection "Keep-Alive"
                                  ]
                     , rqBody = str
                     }                                                               --`debug` ("0s=" ++str)
             
             where str= show os

      --status (MkSocket _ _ _ _ mvstatus)= readMVar mvstatus     

   --setReceiver ::  ([t] -> URIHold -> IO()) ->IO()
   setReceiver  port f= pwrapper port f1 where
     f1 req= do
       let x= read $ rqBody req                                                     `debug` ("received="++rqBody req)
       
       x'<- f x --(URIHold (toString url) url)
       return $ show x'
       
     toString url= (uriToString (\t->t) url) ""     
     
     pwrapper ::  Int ->( Request ->  IO String )-> IO ()
     pwrapper port f = 
       withSocketsDo $ do 
            sock <- listenOn (PortNumber $ fromIntegral port)                       `debug` "listen port"
            acceptConnections f sock

     acceptConnections fn sock = do 
            (sock', addr) <- Network.Socket.accept sock
            forkIOd "accept" $ use sock'
            acceptConnections fn sock
          
      where 
      
      use sock'= do
         
         open <- sIsConnected sock'
         if not open then error  "connection was closed"
           else do

            rrequest <- receiveHTTP sock'
            case rrequest of
             Left error -> print error
                                                
                              
             Right req  -> do 
                modifyMVar_ sockets(\msockets-> let url= rqURI req in return $ M.insert (toString url) (Sock sock') msockets)                                   --`debug` "take socket list"
                let url= rqURI req                                                   
                forkIOd "receive" $ do
                  str <- handle errProcess $ fn req                                  `debug` ("req***="++ show req)
                  respond str                                                        `debug` ("**to respond: "++ str)
                     --xxx checkeo error 
                
                use sock'   
                
                where
                errProcess e = return $ show e
                respond str=do
                  open <- sIsConnected sock'
                  when(not open) $ error "SOCKET NOT OPEN"                           `debug` "connection was closed"
 
                  respondHTTP sock' Response{rspReason="OK"
                                            ,rspHeaders=[Header HdrContentLength  $ show $ length str
                                                        --,Header HdrTransferEncoding "chunked"
                                                        ,Header HdrContentType "text/plain; charset=\"utf-8\""
                                                        --,Header HdrContentEncoding "deflate"
                                                        --,Header HdrConnection "Keep-Alive"
                                                        ]
                                            ,rspBody= str
                                            ,rspCode=(2,0,0)}               `debug` "***sending response****"


