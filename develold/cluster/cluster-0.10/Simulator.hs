{-# OPTIONS -XPatternSignatures #-}
module Simulator where
import System.Random
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Concurrent
import Data.Monoid
import Control.Monad
import Data.List ((\\))

  

data Proc= Proc {pending ::[Msg], lastRec :: Int, consolidated:: Int, ind :: Int} deriving Show
procList :: [MVar Proc]
procList= [unsafePerformIO $ newMVar proc0, unsafePerformIO $ newMVar  proc0{ind=1}] where proc0= Proc [] 0 0 0


type Msg= Int 

seqn= unsafePerformIO $ newMVar (0 :: Int)

genSeq= do
  n <- takeMVar seqn
  putMVar seqn (n+1)
  d <-  getStdRandom (randomR (1, 50)) :: IO Int
  threadDelay d
  print n
  return  n

randomGen= do
   r <- getStdRandom (randomR (1, 5))
   seq<- for r  genSeq []
   return seq
   where
   for 0 _ xs=return xs
   for (n::Int) f xs= do
      msg <- f
      for (n-1) f (mappend xs [msg])
 
instance Eq Proc where
   p == p'= ind p== ind p'
      
proc i= do
  let pr= procList !! i 
  let rest= procList \\ [pr]
  --let rest= take (let n= i -1 in if n <0 then 0 else n) procList 
  --          ++ drop (let len= length procList;n= i+1 in if n == len then len else n) procList

  calcConsolid pr
  for 5 pr rest
  where
  for 0 _ _= return ()
  for n vpr rest= do
 
    xs <- randomGen
    append1 vpr xs
    send xs rest
    for (n-1) vpr rest
    
  send xs nodes= mapM_ (send1  xs) nodes :: IO ()
  
  send1 xs node= do
    append1 node xs :: IO ()
  
  append1 vpr  xs= do
    pr <- takeMVar vpr
    let pend = pending pr
    putMVar vpr $ pr{pending=mappend pend xs} :: IO ()

  calcConsolid vpr= do
    prs <- mapM readMVar procList
    pr <- takeMVar vpr
    let cons= minimum $ map (\p->consolidated p) prs
    putMVar vpr pr{consolidated= cons}
    
  
   
main= do
  h1<- forkIO $ proc 0
  h2<- forkIO $ proc 1
  threadDelay 5000000
  killThread h1
  killThread h2
  
  putStrLn "-------------"
  prs <- mapM readMVar procList
  
  mapM print prs
  where
 