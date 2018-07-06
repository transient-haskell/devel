{-# OPTIONS -fglasgow-exts #-}
module Conf(
Conf(..),getConf,getProjectConf
,Majorities(..)
,getMajorities
,getMajorities1
,hasErrorConfig
,defaultConstitution
,objectPath)
where

import qualified Data.Map as M
--import System.Eval.Haskell
import Control.Concurrent.MVar
import Data(Subject(..),SContent(..))
import Vote(dataPath,justGetVResource,uSubject,ResourceVote(..))
import System.IO.Unsafe
import Data.Array
import Lang(majorities)
import Data.List(elemIndex)
import Data.IORef
import ConfDefs
import Debug.Trace

debug a b= trace b a

objectPath= unsafePerformIO $ newIORef ""

-- replaces Parameter.hs VParameters.hs WebObject.hs

getMajorities :: String -> String -> IO Majorities
getMajorities cat project=  do
   conf <- getProjectConf project `debug` ("getProjectConf, project= "++project)
   return $ getMajorities1 cat conf

getMajorities1 :: String -> Conf -> Majorities
getMajorities1 cat conf = (catMajorities conf) !! indexCategory cat conf where
   indexCategory cat conf= case elemIndex cat $ categories conf of
         Just i  -> i `debug` "majorities index found"
         Nothing -> error $ cat++ " is not a known category"



parameters= unsafePerformIO $ newMVar $ M.empty :: MVar (M.Map String Conf)

getConf :: String -> IO Conf
getConf key= do
  c<- getConfig key readFile
  case c of
    conf@Conf{}-> return conf
    _ -> error $ "conf invalid for key: "++key
getProjectConf :: String -> IO Conf
getProjectConf project= do
  v<- getConfig project readSubject
  case v of
   conf@ProjectConf{} -> return conf
   _ -> error $ "erroneous constitutionf for group: "++ project

getConfig :: String -> (String->IO String)-> IO Conf
getConfig key readConf=do
   path <- readIORef objectPath
   parms <- readMVar parameters
   case M.lookup key parms of
	Just v -> return v
        Nothing->
                  do t <- readConf key
                     v<- (return $ read t)  `catch` (error $ "syntax error compiling: "++ t)
                     {-
                     ev <- eval_ t ["ConfDefs"] [] [][path]
		     let v= case  ev of
                             Right mv ->
				case mv of
                                    Just v -> v
                                    Nothing -> error $ "Nothing compiling: "++ t
                             Left s   -> error $ "syntax error compiling: "++ t ++" : "++ unlines s
                     -}
		     parms <- takeMVar parameters
                     putMVar parameters $ M.insert  key  v parms
                     return v

hasErrorConfig :: String -> Either String Majorities
hasErrorConfig t= unsafePerformIO $
  do
      path <- readIORef objectPath
      ev <- eval_ t ["ConfDefs"] [] [][path] `debug` ("eval "++ show t)
      return $ case ev of
                 Right mv ->
                   case mv of
                      Just (v@ProjectConf{}) -> Right $ getMajorities1 "Constitutional" v
                      _ -> Left "Not valid data" `debug` "Left data"
 	         Left errs-> Left $ unlines errs  `debug` "Left unlines"


readSubject project=  do
	Rs sub <- justGetVResource $ Rs uSubject{sname=Lang.majorities, prosname= project}
	print "readSubject,, sub= "
	print sub
        case  content sub of
          ConfText t -> return t
          _          -> error $ project ++" content is not evaluable"

defaultConstitution= unsafePerformIO.readFile $ dataPath ++ "DefaultConstitution.hs"
