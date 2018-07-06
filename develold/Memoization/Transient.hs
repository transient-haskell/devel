{-# LANGUAGE FlexibleInstances #-}
module Transient where
import Data.Typeable
import qualified Data.Vector as V
import Data.IORef
import Control.Exception.Base
import System.IO.Unsafe
import Control.Concurrent
import System.Time
import Control.Monad.Trans
import Control.Monad.Identity
import Debug.Trace

(!>)= flip trace

getTimeSeconds :: IO Integer
getTimeSeconds =  do
          TOD n _  <-  getClockTime
          return n


type FinalTime= Integer
type EventType= Int
type Delay = Integer

data TType = Event CreationTime EventType
          | Timeout FinalTime
          | Session
          | Pure
          deriving (Eq,Ord)




eval2 r@( Trans ev  mx x) =
 case eval ev of
     (_,True)  -> r
     (ev',False) -> Trans ev' mx (execute mx)

eval ev=let (ev', bools) =  unzip $ map val ev
        in (ev', and bools)
  where
    val Pure= (Pure,True)
    val Session= (Session,True)
    val tout@(Timeout t )=
              let t' = unsafePerformIO getTimeSeconds
              in if t  > t' then (tout,True)
                            else (Timeout t',False)

    val e@(Event t i)= unsafePerformIO $ do
           ev <- readIORef events
           let mr = ev V.! i
           case mr of
             Nothing -> return (e, True)
             Just t' -> if t > t' then  return (e,True)
                                  else  return (e,False)

data Transient m a= Trans [TType] ( m a) a

instance Executable m => Monad  (Transient m ) where
     x >>= f =
            let Trans ttype mx x' = eval2 x
                Trans ttype' my y  = eval2 $  f x'
                ttype''= mix ttype ttype'
            in  Trans ttype''  my y


     return x= Trans [Session] (error "return") x

instance  MonadIO (Transient IO) where
     liftIO iox=
       let x= unsafePerformIO iox
           t= unsafePerformIO getTimeSeconds
       in  x `seq` t `seq` Trans  [Event t iOEvent]  iox x



iOEvent= 0

--data TransientException=  EventException Id | InvalidComputation Id deriving (Show, Typeable)

--instance Exception TransientException

mix [] xs= xs
mix xs []= xs

mix (t:xs) (t':ys) | t == t'= t : mix xs ys
                   | otherwise= if t > t' then t':t:mix xs ys
                                          else t':t:mix xs ys

session :: Executable m => m a -> Transient m a
session mx= Trans [Session] (error "session value must not have been reexecuted") (execute mx)
pure x = Trans [Pure] ( error "pure value must not have been reexecuted") x
eventual ev mx=
       let t= unsafePerformIO getTimeSeconds
       in  t `seq` Trans [Event t ev] mx (execute mx)

timeout delay mx=
       let t= unsafePerformIO getTimeSeconds
       in t `seq` Trans [Timeout $ t + delay] mx (execute mx)

type CreationTime= Integer


events :: IORef(V.Vector (Maybe CreationTime))
events= unsafePerformIO $ newIORef  $ V.fromList [Nothing]

newEvent :: (Executable m, MonadIO m)=> Transient m EventType
newEvent= session $  liftIO $ atomicModifyIORef events $ \v -> (V.snoc v Nothing ,V.length v)


raiseEvent :: (Executable m, MonadIO m)=> EventType -> Transient m ()
raiseEvent ev = session $ liftIO $ do
      t <-getTimeSeconds
      atomicModifyIORef events $ \v -> (v V.// [(ev,Just t)] , ())


class Executable m where
  execute :: m a -> a

instance Executable Identity where
  execute (Identity x)= x

instance Executable IO where
  execute= unsafePerformIO

exec trans= let Trans ev mx x = eval2 trans in return x

transresult=
  exec $ do
       x <- session (return 3)
       y <- timeout 5 $ getTimeSeconds
       return  $ x * y

main= do
  r <- transresult
  print r
  threadDelay 1000000
  main

