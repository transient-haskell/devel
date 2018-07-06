{-# OPTIONS -fglasgow-exts  -XOverlappingInstances -XUndecidableInstances  #-}
{- |
IDynamic is a indexable and serializable version of Dynamic. (See @Data.Dynamic@). It is used as containers of objects
in the cache so any new datatype can be incrementally stored without recompilation.
IDimamic provices methods for safe casting,  besides serializaton, deserialirezation and retrieval by key.

@data IDynamic= forall a. (Typeable a, IResource a) => IDynamic  a deriving Typeable@
-}
module Data.IDynamic where
import Data.Typeable
import Unsafe.Coerce
import System.IO.Unsafe
import Data.IResource
import Data.RefSerialize
import Data.Word
import Numeric (showHex, readHex)
import Control.Exception(handle,SomeException)
import Control.Monad(replicateM)



data IDynamic= forall a. (Typeable a, IResource a) => IDyn  a | IDyns String deriving Typeable


errorfied str str2= error $ str ++ ": IDynamic object not reified: "++ str2

instance  IResource IDynamic  where
   keyResource (IDyn   x) =  keyResource  x
   keyResource (IDyns  str) = errorfied "keyResource" str
   defPath (IDyn  x)= defPath x
   defPath (IDyns str) = errorfied "defPath" str
   serialize (IDyn x)=  {-"Dyn "  ++-} serialize x
   serialize (IDyns str) = {-"Dyn "++ -} str
   deserialize ({-'D':'y':'n':' ':-}str)= IDyns str
{-
   tshowp (IDyn x)= do
           str <- tshowp x
           return $ "Dyn " ++ show (length str) ++ " " ++
            str

   treadp = do
             symbol "Dyn"
             n  <- integer
             str <- replicateM (fromIntegral n) anyChar
             return $ IDyns str
             <?> "IDynamic"
-} 


   
   writeResource (IDyn x)=  writeResource  x
   writeResource (IDyns str) = errorfied "writeResource" str

   readResource (IDyns str) = errorfied "readResource" str
   readResource  d@(IDyn  x)= do
          mx <- readResource x    --`debug` ("retrieving key "++ show (typeOf x))
          case mx of
            Nothing -> return $ Nothing
            Just x  -> return $ Just $ toIDyn x
 
          
instance Serialize IDynamic where
    showp = tshowp
    readp = treadp
    
instance Show  IDynamic where
 show (IDyn x)= "(IDyn "++  serialize x++")"
 show (IDyns str) =  "IDyns " ++ str

-- | DynamicInterface groups a set of default method calls to handle dynamic objects. It is not necessary to derive instances from it



toIDyn x= IDyn x
 
fromIDyn :: (Typeable b, IResource b)  => IDynamic -> b
fromIDyn x=r where
  r = case safeFromIDyn x of
          Nothing -> error $ "fromIDyn: casting failure for data "++ serialize x ++ " to type: " ++  (show $ typeOf r)
          Just v -> v


safeFromIDyn :: (Typeable b, IResource b) => IDynamic -> Maybe b       
safeFromIDyn (IDyn a)= cast a 
safeFromIDyn (IDyns str)= unsafePerformIO $ handle (\(e:: SomeException) -> return Nothing) $
     return . Just $ deserialize str

 






