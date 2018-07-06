{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances  #-}

module Main where

import TCache

import System.IO.Unsafe
import Debug.Trace

debug a b= trace b a
data Stat= Stat{state:: [Int], depth:: Int, index :: [Int], recover:: Bool} deriving (Read, Show)
data WFData a= WFData Stat  a deriving (Show, Read)


class State s where
   getState :: s -> [Int]       -- el estado almacenado cuando paró
   setState :: [Int] -> s ->s   -- set the state
   indexState :: s -> [Int]     -- counter for recovery
   setIndex ::  [Int] -> s -> s
   setRecovery :: s -> s
   recovery :: s -> Bool
   setDepth :: Int -> s -> s
   getDepth :: s -> Int

instance State (WFData a) where
   getState (WFData a _)= state a
   setState  x (WFData a v)= WFData a{state=x} v
   indexState (WFData a _)= index a
   setIndex  x (WFData a v)= WFData a{index= x} v
   recovery (WFData a _)= recover a 
   setRecovery (WFData a v)= WFData a{recover= True} v
   setDepth x (WFData a v)= WFData a{depth= x} v
   getDepth (WFData a _)= depth a
   
instance IResource a => IResource (WFData a) where
   keyResource (WFData s x)= "WFData"++keyResource x

insertResource x=  withResources [] (\_-> [x])


--runWorklow a f= 
        
class Workflow m a where
    (>-) ::  m (WFData a) -> (a->  m a) -> m (WFData a) 
    give ::  (WFData a) -> m (WFData a)
            
instance (Monad m, State a,IResource a) => Workflow m a where
    mx >- f = do
        x <- mx
        let st= getState x
        let ind=  indexState x
        let depth= getDepth x
        let state= if length st < depth+1  then st  ++ [0] else st   
        let index= if length ind< depth+1 then ind ++ [0] else ind  

        if recovery x &&  index !! depth <   state !! depth
                
                   then do
                        return $ setIndex( index // (depth, +1)) x
                                 `debug` ("noop "++show x)
                   else do
                        let x1 = setDepth  (depth +1)  x
                        let f1 = give 
                        x2 <- f x1
                        let stat= take (depth+1) state
                        let x3= (setDepth depth  . setState  (stat // (depth,+1)))  x2 
                        give x3 `debug` ("op "++ show x3)
                        

    give x=  (insertResource  x `debug` "wrote") `seq` return x

(//) ::  [a] -> (Int, (a->a))->  [a]  
(//) xs (n,f)= let (ys,zs)=  splitAt n xs in ys++ ( (f $ head zs):tail zs)

f :: String -> WFData [String] ->  IO (WFData [String])
f x1 (WFData s x)= return $  WFData s (x++[x1])

f1 a@(WFData _ x)= if length x < 3 then f "f1 1" a >-f "f1 2" else f "f1 3" a >- f "f1 4" >- f "f1 5"
--br= exitFailure

instance IResource [String] where
  keyResource _=  "A"

vacia= WFData Stat{state=[] ::[Int], index=[] :: [Int], recover=False , depth=0} []




main=do   
          x <-  f "1" vacia >-f "2" >-  f "3">- f1  >- f "4"
          syncCache (refcache :: Cache (WFData [String]))
          print x


