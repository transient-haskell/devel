-----------------------------------------------------------------------------
--
-- Module      :  CloudSync
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, RankNTypes
    , DeriveGeneric, FlexibleInstances, UndecidableInstances #-}
module CloudSync where

import  qualified Data.HashTable.IO as H
import Data.TCache
import Data.TCache.DefaultPersistence
import qualified Data.Map as M
import Control.Concurrent.MVar
import Data.Typeable
import Control.Monad
import Control.Monad.Primitive
import System.IO.Unsafe
import Data.List(span,insert)
import Data.ByteString.Lazy.Char8(pack,unpack)
import EventSourcing
import Control.Workflow
import Control.Distributed.Process(NodeId)
import Data.List
import Data.Char
import Data.Binary
import GHC.Generics(Generic)
import System.Time(ClockTime)

type SyncGroup= Int

--instance Read NodeId where
--  readsPrec n s= if "nid://" `isPrefixOf` s
--    then
--        let (s',s'')= break (not . isSpace) $ drop 6 s
--        in [(NodeId (read s'), tail s'')]
--    else error "read Nodeid"

instance Binary a => Serializable a where
   serialize = encode
   deserialize = decode


data Stamp = Stamp{stime ::ClockTime , nodeid ::NodeId} deriving (Show, Eq, Ord)

data Event a = Event{stamp :: Stamp, esyncGroup :: SyncGroup, payload :: a} deriving (Show, Typeable)

class Processable a where
   process :: a -> IO ()

data EventList a= EventList{lsyncGroup :: SyncGroup, events:: [Event a]}

eventCache :: forall a.H.BasicHashTable  SyncGroup ([Event a], DBRef Stat)
eventCache= unsafePerformIO $   H.new

newtype Consolidated= Consolidated Stamp deriving (Show, Typeable,Generic)

instance Binary Consolidated

refConsolidated :: DBRef Consolidated
consolidatedKey= "Consolidated"
instance Indexable Consolidated where key= const consolidatedKey
refConsolidated = getDBRef consolidatedKey

addEvent e@Event{..}= do
   (es,st) <- H.lookup  eventCache esyncGroup `onNothing` error "eventCache not initialized"
   Consolidated t <- atomically $ readDBRef refConsolidated `onNothing` error consolidatedKey
   let (tosave, es') = span (< Event{stamp=t}) $ insert e es
   when (not $ null tosave) $ logEvents st tosave
   H.insert eventCache esyncGroup (es',st)

   where
   logEvents st = mapM_ (logEvent st)

newtype Nodes= Nodes (M.Map NodeId Stamp) deriving (Typeable,Show,Generic)

instance Binary Nodes

keyNodes= "Nodes"
instance Indexable Nodes where key= const keyNodes
refNodes :: DBRef Nodes
refNodes= getDBRef keyNodes

--instance (Read a, Show a)=> Serializable a where
--   serialize= pack . show
--   deserialize= read . unpack

data Message a= Message{consolidated:: Stamp, event :: Event a}

processMessage Message{..}= do
  atomically $ do
    Nodes ts <- readDBRef refNodes `onNothing` return (Nodes M.empty)
    let node = nodeid consolidated
    let ts'= M.insert node consolidated ts
    writeDBRef refNodes $ Nodes ts'
    writeDBRef  refConsolidated  . Consolidated . minimum $ M.elems ts'
  addEvent event


