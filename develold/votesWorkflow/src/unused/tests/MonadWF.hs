{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances  #-}
module Main where

import TCache
import System.IO.Unsafe
import Unsafe.Coerce
import Debug.Trace

debug a b = trace b a

data WF m s l = WF { st :: s -> m (s,l) }
data Stat = Stat{state:: [Int], depth:: Int, index :: [Int], recover:: Bool, resource :: String } | I Int deriving (Read, Show)

stat0= Stat{state=[0] ::[Int], index=[0] :: [Int], recover=False , depth=0, resource=""}

instance IResource Stat where
   keyResource Stat{resource= x}= "Stat." ++ x
   keyResource (I n)= "int"

insertResources xs=  withResources [] (\_-> xs)
unsafeIOtoWF x= let y= unsafePerformIO x in y `seq` return y


instance Monad m =>  Monad (WF m Stat) where
    return  x = WF (\s ->  return  (s, x)) 
    WF g >>= f = WF (\s ->do
                         let state1=  state s
                         let index1=  index s
                      
                         if recover s &&  head index1 <   head state1
                               then unsafeCoerce $ g (s {index = head index1 +1:tail index1 })
                               else do
                                    (s', x) <- g s 
                                    let WF fun=  f x 
                                    (s1, x') <- fun s'{state= 0:state1}
                                    let s2= s1{state= head state1 +1 : tail state1} 
                                    unsafeIOtoWF $  print  s2
                                    return (s2, x')
                     )

step :: Monad m => (Stat -> m Stat) -> (Stat -> WF m Stat Stat)
step f= \v -> WF(\s -> do  v'<- f v
                           let s'= s{resource=keyResource v'} 
                           unsafeIOtoWF $ insertResources  [s',v']
                           return (s', v'))

exec :: (Monad m) =>  WF m s v -> s -> m v
exec (WF f) s = do (s', x) <- f s
                   return x 
                   
run :: (Monad m,Monad (WF m s)) =>  (v -> WF m s v) -> s -> v -> m v
run f s v= exec (f v) s

(//) ::  [a] -> (Int, (a->a))->  [a]  
(//) xs (n,f)= let (ys,zs)=  splitAt n xs in ys++ ( (f $ head zs):tail zs)


f:: Stat  -> IO  Stat 
f (I m)= return $ I (m+1)

f1 x= do
        y<- (step  f) x >>= step f >>= step f
        step  f $ y

main=do
      x <-  run   f1   stat0 (I 0)
      print x
     
          
