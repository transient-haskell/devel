module DynModule(rebuild,unload,loadMod) where
import System.Plugins
import Control.Concurrent.MVar 
import qualified Data.Map as M (insert,empty,delete,lookup) 
import Control.Exception
import System.IO.Unsafe
import Data.Maybe(catMaybes)

modules= unsafePerformIO $ newMVar (M.empty)

rebuild source env= makeAll source  $ 
                                  [ "-F", "-pgmFtrhsx",  "-package hsp"  -- , "-package HTTP-3000.0.0"
                                  ,"-fglasgow-exts", "-fallow-overlapping-instances"
                                  ,"-fallow-undecidable-instances"] ++ 
                                  (catMaybes . map (\(s,v)->if s=="ghcoption" then Just v else Nothing) $ env)
                                  

unload mod=do mv <- takeMVar modules
              case M.lookup name mv of
               Just (mod,_) -> do
                          unloadAll mod
                          putMVar modules $ M.delete  mod mv
               Nothing -> return ()

loadMod:: String-> String->IO String          
loadMod file method = do    
       print $ "loadExec" ++ file ++" "++ extension
       mmodules <- takeMVar modules
       return $ case M.lookup file mmodules  of
            Just (_,v) -> v
            Nothing -> do      
               mv <- load file [path] [] method
               case mv of
                LoadSuccess mod v ->  do
                  putMVar modules $ M.insert file (mod,v) mmodules
                  return  v
                LoadFailure msg -> error msg
                  
            
           

