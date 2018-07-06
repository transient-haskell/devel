module Cluster.Debug where
import Debug.Trace
import Data.Map as M
import Control.Concurrent
import System.IO.Unsafe
import Control.Concurrent.MVar

list :: MVar (Map String String)
list = unsafePerformIO $ newMVar $ empty 





debug a b= trace (f b) a where
 f str= let th = unsafePerformIO $ myThreadId
            str1= show th
            str3= case M.lookup  str1 (unsafePerformIO $ readMVar list) of
                   Just str2 -> str2
                   Nothing   -> ""
        in str1++" "++str3 ++": "++str++"\n"


forkIOd name pr= do
       th <-forkIO pr
       l <- takeMVar list
       case M.lookup (show th) l of
         Just _ -> do
                   putMVar list l
         _      -> do
                   putMVar list $ insert (show th) name l

       return th  