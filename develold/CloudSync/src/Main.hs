{-# LANGUAGE TemplateHaskell,DeriveDataTypeable, RecordWildCards, DeriveGeneric #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.TCache
import Data.TCache.DefaultPersistence

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Binary
import Control.Concurrent
import Data.Typeable
import GHC.Generics
import CloudSync
import Text.Printf
import System.Time
import DistribUtils
import Control.Monad

newtype Seq= Seq Int deriving (Read, Show, Typeable, Generic)
instance Indexable Seq where key= const keyseq
keyseq= "Seq"
refSeq= getDBRef keyseq

instance Binary Seq





server :: Process ()
server = do
  e@Event{..} <- expect                             -- 1
  say $ printf "ping received from %s" (show $ nodeid stamp) -- 2
  addEvent (e :: Event Int)



sgroup = 1

client :: [NodeId] -> Process ()
client peers = do
  ps <- forM peers $ \nid -> do                      -- 2
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'server)

  mypid <- getSelfPid

  loop ps
  where
  loop mypid ps= do
      forM_ ps $ \to -> do
          time <- liftIO $ getClockTime
          liftIO $ atomically $ do
            Seq seq <- readDBRef refSeq `onNothing` return (Seq 0)
            writeDBRef refSeq $ Seq (seq +1)
          send to Event{stamp=Stamp time mypid ,esyncGroup=sgroup,payload= seq}
      liftIO $ threadDelay 1000000
      loop mypid ps

remotable [ 'server ]

main = distribMain  client Main.__remoteTable
