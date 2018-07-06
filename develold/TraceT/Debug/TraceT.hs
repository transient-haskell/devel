{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction -XOverlappingInstances  -XUndecidableInstances #-}
module Debug.TraceT(
   debug
 , trace
 , debug1
 , Traceable(..)
 , (!>)
)
where
import qualified Debug.Trace as T
import System.IO.Unsafe
import Time
import Control.Concurrent(myThreadId)
import Control.Concurrent.STM (STM)

data CClockTime= T Integer Integer deriving Show

debug1= flip  . T.trace

debug :: x -> String -> x
debug a b= Debug.TraceT.trace b a

trace b a= T.trace (f b) a where
 f str= unsafePerformIO $ do
                 th <- myThreadId
                 time <- getTime
                 return $ show time ++ " "++ show th ++": "++str++"\n"

--debug1 x str =     debug x (str++" return= "++ show x)

getTime  = getClockTime
  {-  do
            TOD t1 t2 <- getClockTime
            return $ T t1 t2
-}

class Traceable x where
    debugf  ::  x -> String -> x

(!>)= debugf

instance Show x => Traceable (IO x)  where
   debugf iox str = do
              x <- iox
              return $ debug x (str++" => IO $ " ++ show x)

instance Show x => Traceable (STM x) where
  debugf stmx str=   do
              x <- stmx
              return $  debug x (str++" =>STM $  " ++ show x)


{-
instance  ( Monad m,Show x) => Traceable (m x) where
  debugf stmx str=   do
              x <- stmx
              return $  debug x (str++" =>m $  "++ show x)

-}
instance Show x => Traceable x where
   debugf x str =     debug x (str++"= "++ show x)

