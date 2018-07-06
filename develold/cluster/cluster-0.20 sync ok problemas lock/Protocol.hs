{-# OPTIONS  -fglasgow-exts  #-}
module Protocol where

class Protocol t nodeId | t ->nodeId, nodeId -> t where
   send :: t -> nodeId -> IO (Maybe t)
   setReceiver :: Int -> (t -> IO t) ->IO()
   
 --remoteExec
 --exec
 --redirect