{-# OPTIONS -fglasgow-exts #-}
module Init where

import GHC.ConsoleHandler
import System.Exit
import System.IO.Unsafe
import GHC.Conc
import Control.Exception as Exception
import Control.Concurrent
import Network
import Data.List(lookup)
import Data.Maybe(fromJust)
import Data.List(elemIndices)
import Data.IORef
import System.Directory(getCurrentDirectory)
import Conf(objectPath,getConf,Conf(..))
import Data
import Vote
import HTTPParser((->>))

import TCache

import Aprobal (cronList,startCronTab)

import Search(readIndex)


defaultConf=  Conf{
        syncCacheTime= 60,     
	sizeCache= 200     }
 

getConf1=  Exception.catch (getConf conf) 
             (\e->do 
		    writeFile conf $ show defaultConf
		    logEntry  "using default configuration"
		    return defaultConf)
	where
	  conf= dataPath++config
	






setObjectPath env =  do
   s <- readIORef objectPath
   case s of
     ""-> do
           dir <- getCurrentDirectory
	   let path1= dir++path
	   logEntry $ "object path= "++ path1
	   writeIORef objectPath path1
	   return ()
	   where
	   (path,_)= splitAt (last xs+1) script
	   xs=elemIndices '/' script 
	   script= env->>"SCRIPT_NAME" 
	   

     _->  return () 




initialize env= do 
        setObjectPath env 
        if not initialized  then error "FreeChooser not initialized" else return ()	


initialized =  unsafePerformIO $  do
        print "--------------INITIALIZE VOTES-------------------"        
        
	checkRepository
	conf <- getConf1 	

	let timeout= syncCacheTime conf
	logEntry $ "cache synconization every "++ show timeout ++ " seg."

	let sizeCache1= sizeCache conf
	logEntry $ "cache size= "++show sizeCache1++" elems"

	-- to syncronize the cache with data
	clearSyncVCacheProc timeout defaultCheck sizeCache1 

	logEntry "Starting the search index"
	readIndex indexFname

	logEntry  "Starting crontab processes"
	startCronTab

	
	--installHandler $ Catch  (\qe->do{syncVCache;exitWith ExitSuccess})
        print "--------------END INITIALIZE VOTES-------------------"

        return True

    
