{-# OPTIONS -fglasgow-exts #-}
module Main where
import Data.Typeable
import Data.Vector
import Data.IORef
import Control.Exception.Base
import System.IO.Unsafe
import Control.Concurrent
import System.Time
import Control.Monad.Trans

-- como hacer que distintos eventos modifiquien una misma variable

ioinfluences :: IORef (Vector  [IORef Bool])
ioinfluences = unsafePerformIO $ newIORef empty

type Id= Int

main= do
           runTransient $  do
              t <- each10Seconds
              liftIO $  print t
              liftIO $ threadDelay $ 20 * 1000000
              liftIO $  print t

returnIO :: Transient a -> IO a
returnIO (Trans (_,x))= x `seq` return x

runTransient f=  withTransientData  $ returnIO  f

withTransientData  f =
 handle (\(e :: TransientException)-> do
        case e of
          EventException id -> do
                              print "event"
                              influences <- readIORef ioinfluences
                              let l= influences ! id
                              mapM (writeIORef'  False) l
                              writeIORef ioinfluences influences
                              f
          InvalidComputation _ -> f)
        f
        where
        writeIORef' = flip writeIORef
{-
donde se pone el handler? en e topLevel
transientHandlerStart
hay que tener en cuenta que el calculo ya ha podido ser hecho
-}

each10Seconds :: Transient Integer
each10Seconds = every 10 $  unsafePerformIO $ getTimeSeconds

every :: Int -> a ->  Transient a
every  n x=unsafePerformIO $ do
  (n , ioref)<-  invalidator
  return $  Trans (Event n  ioref, x)

  where

  invalidator=   do
    influences <- readIORef ioinfluences
    ioref <-  newIORef True
    writeIORef ioinfluences $ snoc influences  [ioref]
    let id= Data.Vector.length  influences
    pid <- myThreadId
    forkIO $ do
       threadDelay  $ n * 1000000
       throwTo pid $ EventException id
    return (id, ioref)

getTimeSeconds :: IO Integer
getTimeSeconds =  do
          TOD n _  <-  getClockTime
          return n

influence id ioref = do
       influences <- readIORef ioinfluences
       let l=  influences ! id
       writeIORef  ioinfluences $ update influences (singleton( id, (ioref : l)))



-----------------------
--(Vector  [IORef Bool])
{- withTransientData f= do
                id <- computationID
                -- como avisar a los invalidator que avisen a un id determinado?


-}


data TType= Session | Event Id (IORef Bool)

data Transient a= Trans (TType, a)

instance Monad  Transient where
     Trans( ev,x) >>= f =
                  let  Trans (ev', y)    =  f  x
                  in   Trans ((min1 ev ev') , y)

     return x = Trans (Session, x)

instance MonadIO Transient where
     liftIO iox= let x= unsafePerformIO iox in  x `seq` Trans (Event 0 (unsafePerformIO $ newIORef False),  x)

data TransientException=  EventException Id | InvalidComputation Id deriving (Show, Typeable)

instance Exception TransientException


min1 (Event n ioref )(Event  n' ioref') = unsafePerformIO $ do
                           valid  <- readIORef ioref
                           valid' <- readIORef ioref'
                           if not (valid && valid')
                               then  throw $  InvalidComputation n
                               else do
                                   influence n'  ioref
                                   return $  Event n ioref



{-
liftT:: Time -> IO a -> Transient a
liftT time  p=   Trans time $ unsafePerformIO p

liftTS= < whatever memoization code>
liftTS2=....
liftTS3=....

Memorization then is embedded in the monad transformation
This may be more elegant than  IO plus unsafePerformIO and is more informative for the programmer. The transition form IO to pure can be done in two steps, see below

  Instead of unsafePerformIO,  we can use:

unsafePurifyTransient :: Transient a -> a
unsafePurifyTransient  (Tran Session x) = x
unsafePurifyTransient _ = userError ...

for the inherently transient calls

A safer version of unsafePerformIO using implicit memoization could be:
unsafePerformIOT :: IO a -> a
unsafePerformIOT = unsafePurifyTransient  .  liftT

unsafePerformIOT guatantee that it returns the same value in the same session.

-}
