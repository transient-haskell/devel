{-# OPTIONS -fglasgow-exts  -XUndecidableInstances -XBangPatterns #-}

{- |
IDynamic is a indexable and serializable version of Dynamic. (See @Data.Dynamic@). It is used as containers of objects
in the cache so any new datatype can be incrementally stored without recompilation.
IDimamic provices methods for safe casting,  besides serializaton, deserialization, registrations and retrieval by lkey.

@data IDynamic= forall a. (Typeable a, IResource a) => IDynamic  a deriving Typeable@
-}
module Data.TCache.IDynamic where
import Data.Typeable
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Concurrent.MVar 
import Data.Map as M
import Data.TCache.IResource
import Data.RefSerialize
import Data.HashTable(hashString)
import Data.Word
import Numeric (showHex, readHex)


data IDynamic= forall a. (Typeable a, IResource a) => IDynamic  a deriving Typeable


list :: MVar (Map Word  (IDynamic -> IO (Maybe IDynamic), String -> IDynamic,  ST IDynamic ) )
list = unsafePerformIO $ newMVar $ empty 

hash x= unsafeCoerce . hashString  . show $ typeOf x :: Word
instance  IResource IDynamic  where
   keyResource (IDynamic  x)=  keyResource  x 
   serialize (IDynamic x)= "Dyn " ++ showHex (hash x)  (  " " ++ serialize x)
   deserialize str2=
           let
                str= drop 4 str2
                [(t :: Word, str1)]= readHex   str

           in
             case M.lookup t (unsafePerformIO $ readMVar list) of
                           Nothing    -> error $ "not registered type " ++ str1 ++ " please registerType it"
                           Just (_, f, _)-> f  $ tail str1

   tshowp (IDynamic x)= do
           str <- tshowp x
           return $ "Dyn " ++ showHex (hash x)  ( " "++ str)

   treadp = do
             symbol "Dyn"
             t  <- readHexp

             case M.lookup t (unsafePerformIO $ readMVar list) of
                           Nothing    -> fail $ "not registered type please registerType it"
                           Just (_,_, f)->  f
            <?> "IDynamic"
            
   defPath (IDynamic x)= defPath x

   writeResource (IDynamic  x)=  writeResource  x

   readResource  d@(IDynamic x)
     | typeOfx==  typeOf Key= do
                                          mx <- readResource x    --`debug` ("retrieving key "++ show (typeOf x))
                                          case mx of
                                            Nothing -> return $ Nothing
                                            Just x  -> return $ Just $ toIDyn x
     | otherwise= 
        case M.lookup  type1 (unsafePerformIO $ readMVar list) of
                           Nothing    -> error $ "not registered type " ++ show (typeOf x) ++ " please registerType it"
                           Just (f ,_,_)-> f  d
           where
           typeOfx= typeOf x
           type1= unsafeCoerce $ hashString $ show typeOfx :: Word
          

instance Show  IDynamic where
 show (IDynamic x)= "(IDynamic \""++show (typeOf x) ++"\" "++  serialize x++")"
  

-- | DynamicInterface groups a set of default method calls to handle dynamic objects. It is not necessary to derive instances from it

class DynamicInterface  x where
     toIDyn :: x     -> IDynamic     -- ^ encapsulates data in a dynamic object
     registerType ::   IO x                -- ^ registers the deserialize, readp and readResource methods for this data type
     fromIDyn :: IDynamic -> x    -- ^ extract the data from the dynamic object. trows a user error when the cast fails
     unsafeFromIDyn :: IDynamic -> x      -- ^ unsafe version.
     safeFromIDyn :: IDynamic -> Maybe x     -- ^ safe extraction with Maybe

 
instance (IResource x,Typeable x) => DynamicInterface  x where


 toIDyn x= IDynamic  x
 
 registerType = do
 
       let x= unsafeCoerce 1 :: x

       let deserializex str= toIDyn (deserialize str :: x)
       let treadpx = do
                    t<-  treadp  :: ST x
                    return  $  toIDyn t
       let readResourcex (IDynamic s)= do
             mr <-  readResource (unsafeCoerce s :: x) :: IO (Maybe x)
             case mr of
                  Nothing -> return Nothing
                  Just s' -> return $ Just $ IDynamic  s' 
       l <- takeMVar list

       let key= hash x

       case M.lookup key l of
         Just _ -> do
                   putMVar list l
                   return x
         _      -> do
                   putMVar list $ insert key (readResourcex, deserializex, treadpx )  l
                   return x


       
 fromIDyn d@(IDynamic  a)= if type2 == type1 then v
                        else error ("fromIDyn: casting "++ show type1 ++" to type "++show type2 ++" for data "++ serialize a)
           where 
           v=  unsafeCoerce a :: x
           type1= typeOf a
           type2= typeOf v
 
 unsafeFromIDyn (IDynamic  a)= unsafeCoerce a

 safeFromIDyn (IDynamic a)= let v=  unsafeCoerce a :: x in if typeOf a == typeOf v then  Just v else Nothing

{- | Key datatype can be used to read any object trough the Dynamic interface.

        @ data Key =  Key 'TypeRep' String deriving Typeable @

         Example
         
        @  mst <- 'getDResource' $ 'Key' type 'keyofDesiredObject'
             case mst of
               Nothing -> error $ \"not found \"++ key
               Just (idyn) ->  fromIDyn idyn :: DesiredDatatype}@
-}

data Key =  Key TypeRep String deriving Typeable


instance  IResource Key  where
  keyResource (Key _ k)=k
  serialize _= error "Key is not serializable"
  deserialize _= error "Key is not serializable"
  writeResource _= error "Please don't create Key objects"
  readResource  key@(Key t _)= 
       case M.lookup  type1 (unsafePerformIO $ readMVar list) of
                           Nothing -> error $ "not registered type "++show t++" please registerType it"
                           Just (f,_,_)   -> do
                                         d <- f . toIDyn $ key
                                         return $ dynMaybe d
       where
       dynMaybe (Just dyn)= return $ fromIDyn dyn
       type1= hash t
