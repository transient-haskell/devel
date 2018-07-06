{-# OPTIONS -fglasgow-exts   #-}
module CachedFunc (Cached,cached,tcached,initCachedTypes) where

import Control.Exception(assert,handle)
import Control.Concurrent(forkIO,threadDelay,killThread,ThreadId)
import Data.Maybe (isJust, fromJust)
import Data.TCache.Dynamic
import Data.TCache.IDynamic
import System.Mem.StableName
import System.IO.Unsafe
import Debug.Trace
import Data.Typeable
import Unsafe.Coerce
import Debug.Trace
import Control.Concurrent.STM(TVar,atomically, readTVar, writeTVar)
import GHC.Conc(unsafeIOToSTM)


debug a b= trace b a

justGetResource r = getResource r >>= (\r -> return $ assert (isJust r) (fromJust r))


data Cached a  b=  Cached a (a -> IO b) b ThreadId 

instance Typeable (Cached a  b) where
 typeOf (Cached _ _ _ _)= mkTyConApp (mkTyCon "#CachedFunc") []

instance  IResource a => IResource (Cached a  b) where
  keyResource ch@(Cached a f _ _)= keyResource a ++ varName f   --`debug` ("k="++ show k)
    where
    {-# NOINLINE varName #-}
    varName x= show . unsafePerformIO $    makeStableName $! x >>= return . hashStableName 

       
  defPath _= "Cached/"
  writeResource _= return ()
  delResource _= return ()
  readResource (Cached a f _ _)= f a >>= \b-> return . Just $ Cached a f b undefined
  serialize _= undefined            
  deserialize _= undefined       

initCachedTypes= do     
   registerType :: IO Data.TCache.IDynamic.IDynamic
   registerType :: IO (Cached undefined  undefined)
   return ()

   
cached ::  (a-> IO b) -> a -> IO b  
cached f a= do
        IDynamic x'  <- justGetResource (IDynamic (Cached a f undefined undefined )) 
        let Cached _ _ b _ = unsafeCoerce x' 
        return b


--x= IDynamic(Cached (1::Integer) (\_-> return 1) undefined undefined)

reference x= do
   [mtv] <-  atomically $ getTVars [x]
   case mtv of 
     Nothing -> do 
        getDResource x
        reference x
     Just tv -> return tv
     

   

-- | memoize the result of a computation for a certain time. This is useful for  caching  heavy pieces of data
-- such are web pages composed on the fly.
     
tcached :: Int -> (a -> IO b) -> a -> IO b
tcached time  f a= do
{-
   tv <- reference $ IDynamic (Cached a f undefined undefined ) 
   atomically $ do

     IDynamic x<- readTVar tv
-}
     IDynamic  x <- justGetResource $ IDynamic (Cached a f undefined undefined ) 
     let cho@(Cached _ _ b th)= unsafeCoerce x
     --th' <-   do 
                 --killThread th
                 --forkIO $ delete cho

     --let x= IDynamic (Cached a f b th')
     --writeTVar tv x 
     return b
   where
   delete cho = do
     threadDelay $ time * 1000000
     deleteResource cho
