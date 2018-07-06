-----------------------------------------------------------------------------
--
-- Module      :  EventSourcing
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

module EventSourcing where

import Control.Workflow
import Data.Typeable
import Data.RefSerialize
import Data.Monoid
import Control.Concurrent.STM
import Data.TCache
import Data.TCache.Memoization
import Control.Concurrent.MVar
import Control.Concurrent

processEvents ops= do
       r<- mapM processEvent  ops
       return   $ last r

processEvent
  :: (Typeable b, Indexable b, Serialize b, Monoid b)
  => b -> IO b
processEvent t = do
   let k= key t
   (res,st) <- atomically . getEventValue $ EventId k
   logEvent st t
   let res'=  res <> t
   atomically $ writeCached k (const $ rebuild k) (res',st) 0
   return res'

--processEventSTM
--  :: (Typeable b, Indexable b, Serialize b, Monoid b)
--  => b -> STM b
--processEventSTM t= do
--   let k= key t
--   (res,st) <- getEventValue k
--   unsafeIOToSTM $ logEvent st t -- should not retry and no duplicate write since
--   let res'=  res <> t
--   writeCached k (const $ rebuild k) (res',st) 0
--   return res'

logEvent st x= stepExec st $ return x

getEventValue :: (Serialize a,Typeable a, Monoid a) => EventId a -> STM (a, DBRef Stat)
getEventValue (EventId key)= cachedByKeySTM key 0 (spawn $ rebuild  key) -- !> "getEvent Value"

spawn p= do
  mv <- newEmptyMVar
  forkIO $  p >>= putMVar mv
  takeMVar mv

rebuild :: (Serialize a,Typeable a, Monoid a)=> String -> IO (a, DBRef Stat)
rebuild k = exec1nc k $ rebuild1 mempty
 where
 rebuild1 res= do
   is <- isInRecover
   if not is
     then do
        st <- getWFStat
        return (res,st)   -- !> "rebuild"
     else do
        t <- step $  undefined
        let res' = res <> t
        rebuild1 res'

{-
problema mempty que tenga key y cumpla las leyes de monoid
mempy <> x= x
x <> mempty= x
pero processEvent necesita primer evento
  primer evento Votes "" [] M.empty

ErrorEvent cuando una sequencia no estaba en teoria permitida.
   se puede ignorar
-}

data EventId a= EventId String deriving (Read, Show)

--instance Indexable a => Show (EventId a) where show (EventId x) = "EventId "++ show x
--instance (Typeable a, Indexable a, Serialize a, Monoid a)=> Read (EventId a) where
-- readsPrec n s= readsPrec1 n $ dropWhile isSpace s
--  where
--  readsPrec1 n  ('E':'v':'e':'n':'t':'I':'d':' ':r)=
--    let [(k,s1)]= readsPrec n r
--    in [(EventId  k,s1)]

--newEvent= fmap EventId newFlow
