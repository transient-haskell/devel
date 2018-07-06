{-# OPTIONS -fglasgow-exts #-}
module WebServer((->>), pwrapper,CgiOut(..),scheduler2)  where

import Network
import Network.Socket
import System.IO
import System.Process
import Control.Concurrent
import Control.Exception(catch, finally)
import System.Time(getClockTime, toUTCTime, calendarTimeToString)

import System.Environment(getArgs       )
--import System.Plugins
import System.Exit

import Data.List(elemIndex, elemIndices, isPrefixOf, lines,(!!))
import Data.Maybe(catMaybes)
import qualified Data.Map as M (insert,empty,delete,lookup) 
import Control.Concurrent.MVar
import Data.Char(isUpper,isSpace)
import System.IO.Unsafe


import HTTPParser

import CGI

import FreeChooser   -- provisional

import Debug.Trace
debug a b= trace b a


-- read n bytes from handle h
readn h 0= return []

readn h n=do
{-	
        str <- hGetContents h
        -- force to read n elements
        if (str !! (n-1))== '\x00' then return str else return str
-}


	c<-hGetChar h 
	rest<-readn h (n-1)
	return (c:rest)

 


pwrapper ::  Integer ->( [(Name,Value)] ->  IO String )-> IO ()
pwrapper port f = 
       withSocketsDo $ do 
            sock <- listenOn (PortNumber $ fromInteger port)
           
            acceptConnections fn sock
          
  where
  
	fn h = do

		qs <- getRequest h --hGetContents h  --readall "" h
		headr<- return   $ header qs 
		body<- readn h (len headr)  

		let env = deblank $ (if (headr ->> "REQUEST_METHOD") =="POST" 
                  					then urlDecode  body 
                   					else urlDecode (headr->>"QUERY_STRING")
                  			)++headr++splitCookies (headr->>"Cookie")
		content <- f env
                			
		
                let len= length content
                --print $ "len="++ show len
		let header= responseHeader (env->>"op"++env->>"project"++env->>"subject")  len
                hPutStr h header
                --print  content 
		hPutStr h content
		

	getContent s []= s
	getContent s (x:xs)= getContent (s++[x]) xs
	

deblank []= []  -- to strip the surrounding whitespaces
deblank ((n,v):rr)= (n,strip v): deblank rr
	where 	strip "" = ""
	        strip " "=""
		strip (' ':v)= strip2 v
		strip v=       strip2 v
		
		strip2 v= if not.isSpace $ last v  then v 
		                         else reverse $ tail $ reverse v


myreads1 "" _   =("","")
myreads1 (c:rest) f  | not(f c)  = ("",rest) 
                     | otherwise = (c:r, s)
                                where (r,s) = myreads1 rest f

len head= case (head->>"Content-Length") of
		"" -> 0
		s  -> read s

lengthHead head= case (head->>"Content-Length") of
		"" -> 0
		s  -> read s

responseHeader tag len=	
		"HTTP/1.1 200 OK\n"++
		"Date: "++strtime++"\n"++
		"Server: Haskell Web Server\n"++
		"Last-Modified: "++strtime++"\n"++
		--"Etag: \""++tag++"\"\n"++
		"Accept-Ranges: none\n"++
		"Content-Length: "++show len++"\nConnection: close\n"


	where	strtime= show $ unsafePerformIO getClockTime

request_method= "REQUEST_METHOD"
script_name=  "SCRIPT_NAME"
query_string= "QUERY_STRING"

header qs=
            (request_method,method)
           :(script_name,cgi)
           :(query_string,cmdline)
           :tail l 
   where

	(h,_)= head l
	(method,r)= myreads1 h (/=' ')
	(cgi,cmdline,http)= case myreads1 r (\c ->c /='?' && c/=' ' && c/='\n') of
				(c,[]) ->(c,"","")
				
				(c,r2) -> if r3=="" then(c,"",cmd) else (c,cmd,http) where (cmd,r3)= myreads1 r2 (/=' ')

	l=map ((flip myreads1) (/= ':')) qs

getRequest :: Handle -> IO [String]
getRequest h = do
  l <- hGetLine h
  if (emptyLine l ) 
     then getRequest h
     else getRequest' l h

getRequest' l h = do
  if ( emptyLine l ) 
     then return []                     -- do{ x <- hGetLine h;return [x]}  
     else do l' <- hGetLine h
	     ls <- getRequest' l' h
	     return (l:ls)

emptyLine "\r" = True
emptyLine _    = False



acceptConnections fn sock = do
  (h, x) <- accept' sock
  let t = case x of
                SockAddrInet port host	->      1
                SockAddrInet6 port ffinfo hostaddr scope ->	2
                SockAddrUnix str -> 3
  print "addr type "
  print t
  forkIO (fn h `finally` (hClose h))
  --yield
  acceptConnections fn sock

accept' :: Socket 		-- Listening Socket
       -> IO (Handle,SockAddr)	-- StdIO Handle for read/write
accept' sock = do
 (sock', addr) <- Network.Socket.accept sock     --Socket.accept
 handle	<- socketToHandle sock' ReadWriteMode
 return (handle,addr) `debug` ("handle, addr="++show handle++","++ show addr)



--------------------------------------------SCHEDULER -----------------------------------


scheduler2 :: [(String,String)] -> IO  String

scheduler2 env = Control.Exception.catch (logscheduler env) (errorHandle env)where 
    errorHandle env e=do
	return $ errorPage e 

    logscheduler env = do
	scheduler env

noCookies= []
errorPage e= show $ Content (RawMime "text/html" $ "<b>An error has occurred: " ++ show e ++ "</b>") noCookies

typeMime  ext= case lookup  ext mimeTable of
    Just s-> s
    _     -> "unknown/"++ext
  where
  mimeTable= [("html","text/html")
             ,("jpg","image/jpeg")
             ,("gif","image/gif")]

pathNameExtension :: String -> (String,String,String,String)
pathNameExtension script=(path,name,ext,rebuild) where
   (path,nameext)= case elemIndices '/' script of
     [] -> ("./",script)
     xs -> splitAt (last xs+1) script
                     
   (name, t)= case elemIndex '.' nameext of
      Just i  -> (name,tail r) where (name, r)= splitAt i nameext
      Nothing -> (name,"")
      
   (ext,rebuild)= case elemIndex '.' t of
      Just j -> ( ext,tail rebuild) where (ext, rebuild)= splitAt j t
      Nothing-> (t,"")    

{-
pathNameExtension :: String -> (String,String,String,String)
pathNameExtension script= do p <- papply pathNameExtensionParser  script
                             let (s,_) = head p
                             return s
                              
                            
        where
      
        pathNameExtensionParser:: Parser (String,String,String,String)
        pathNameExtensionParser= do
                  path <-path 
                  name <-many alphanum
                  char '.'
                  extension <-many alphanum
                  rebuild <- (do char '.'
                               s<-many alphanum
                               return s)
                           `mplus` (string "")
                                       
                  return (path,name,extension,rebuild)
        
 
        path:: Parser(String)   
        path= many $ do
                     many(alphanum) 
                     char '/'

-}






scheduler :: [(String,String)] -> IO String
scheduler env = do 
    print "web scheduler"
    scheduler1 env
    
scheduler1 env

    | script == "" || script == "/" = 
                      FreeChooser.cgi1 env  --scheduler ((script_name,"/default.html"):env)
                      
    | (scriptObject, extension) ==  ("./FreeChooser.o","cgi") = FreeChooser.cgi1 env   -- to link directly
                 
 
{-                       
    | extension == "hs" || extension== "lhs" = do
       mv <- rebuildModule
       case mv of
             MakeFailure errors -> return . show $ Content (RawMime("text/plain") $ concat  errors) noCookies
             MakeSuccess code file -> loadExec (path++name++".o") "cgi"

    
    | extension == "o"  = do
               loadExec script1 "cgi"
                            
    -- "Module.method" syntax
    | isUpper $ head name= 

         case rebuild of
           "debug" -> do
                  unload
                  print "rebuilding"
                  mv <- rebuildModule
                  print "rebuild done" 
                  case mv of
                     MakeFailure errors -> return . show $ Content (RawMime("text/plain") $ concat  errors) noCookies
                     MakeSuccess code file -> loadExec scriptObject extension

                        
           _ -> case (scriptObject, extension)   of
                  ("./FreeChooser.o","cgi") -> cgi1 env   -- to link directly
                  _ -> loadExec scriptObject extension           
 
-}
         
    | otherwise= do print script
                    print  $ typeMime  extension
                    h <- openBinaryFile script1 ReadMode
                    n <-hFileSize h
                    let int = fromIntegral n
                    str <- readn h int
                    hClose h
                    
                    return . show $ Content (RawMime scriptMime str) noCookies
    
  where
    scriptObject= path++name++".o"
    (path,name,extension,rebuild)=  pathNameExtension script1
    script1= tail script
    scriptMime= typeMime extension 
    script= env->>script_name
    
{-
    rebuildModule :: IO MakeStatus
    rebuildModule= makeAll (path++name++".hs")  $ 
                                  [ "-F", "-pgmFtrhsx",  "-package hsp"  -- , "-package HTTP-3000.0.0"
                                  ,"-fglasgow-exts", "-fallow-overlapping-instances"
                                  ,"-fallow-undecidable-instances"] ++ 
                                  (catMaybes . map (\(s,v)->if s=="ghcoption" then Just v else Nothing) $ env)
                                  
    modules= unsafePerformIO $ newMVar (M.empty)

    unload=do mv <- takeMVar modules
              case M.lookup name mv of
               Just (mod,_) -> do
                          unloadAll mod
                          putMVar modules $ M.delete  scriptObject mv
               Nothing -> return ()
    loadExec:: String-> String->IO String          
    loadExec file method = do    
       print $ "loadExec" ++ file ++" "++ extension
       mmodules <- takeMVar modules
       r <- case M.lookup scriptObject mmodules  of
            Just (_,v) -> Control.Exception.catch((valueOf v) env)(return . errorPage)
            Nothing -> do      
               mv <- load file [path] [] method
               case mv of
                LoadSuccess mod v ->  do
                  print "success link"
                  putMVar modules $ M.insert scriptObject (mod,v) mmodules
                  Control.Exception.catch((valueOf v) env)(return . errorPage)
                LoadFailure msg -> return . show $ Content (RawMime("text/plain") $ concat msg) noCookies

       case "Content-type:" `isPrefixOf` r of
              
              True -> return r 
              False-> (return . show $ Content (RawMime("text/html") $ r) noCookies) 
            
-}           
 

