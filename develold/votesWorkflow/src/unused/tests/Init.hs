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

import Data
import Vote
import HTTPParser((->>))

import VCache
import Data.TCache.Dynamic(registerType,defaultCheck)



--import Search(readIndex)
  


defaultConf=  Conf{
        syncCacheTime= 60,     
	sizeCache= 200,
	defaultConstitution= "defaultConstitution",
	objPath= "."     }

 
getConf1 = do
   mconf <- getVResource (Rc undefined) 
   case mconf of
     Just (Rc conf) -> return conf
     Nothing -> do
                logEntry  "using default configuration"
                addVResource (Rc defaultConf)
                return defaultConf
initialize1 :: IO ()
initialize1= if  initialize then return() else error "initialize error"

initialize =  unsafePerformIO $  do
        print "--------------INITIALIZE VOTES-------------------"        
        registerType ( Rs uSubject :: Data.ResourceVote)
        conf <- getConf1 	

	let timeout= syncCacheTime conf
	logEntry $ "cache synconization every "++ show timeout ++ " seg."

	let sizeCache1= sizeCache conf
	logEntry $ "cache size= "++show sizeCache1++" elems"

	-- to syncronize the cache with data
	clearSyncVCacheProc timeout defaultCheck sizeCache1 

--	logEntry "Starting the search index"
--	readIndex indexFname

	--logEntry  "Starting crontab processes"
	--startCronTab

	
	--installHandler $ Catch  (\qe->do{syncVCache;exitWith ExitSuccess})
        print "--------------END INITIALIZE VOTES-------------------"

        return True

    
