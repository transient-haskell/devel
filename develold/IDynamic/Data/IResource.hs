{-# OPTIONS -fglasgow-exts -XDeriveDataTypeable #-}
module Data.IResource where

import System.Directory
import Control.Exception as Exception
import System.IO.Error
import Data.List(elemIndices)
import System.IO
import Control.Monad(when,replicateM)
import qualified Data.RefSerialize as RS
import Data.Typeable
import Data.HashTable(HashTable)
--import System.Time
import Data.Char(isSpace)


instance  (Typeable a, Typeable b) => Typeable (HashTable a b) where
       typeOf _=mkTyConApp (mkTyCon "Data.HashTable.HashTable") [Data.Typeable.typeOf (undefined ::a), Data.Typeable.typeOf (undefined ::b)]

--import Debug.Trace

--debug a b= trace b a

{- | Interface that must be defined for every object being cached.
 'readResource' and 'writeResource' are implemented by default as read-write to files with its key as filename
 'serialize' and 'deserialize' are specified just to allow these defaults. If you define your own persistence, then
 @serialize@ and @deserialize@ are not needed. The package 'Workflow' need them anyway.

minimal definition: keyResource, serialize, deserialize

While serialize and deserialize are agnostic about the way of converison to strings, either binary or textual, treadp and
tshowp use the monad defined in the RefSerialize package. Both ways of serialization are alternative. one is defined
by default in terms of the other. the RefSerialize monad has been introduced to permit IResource objects to be
serialized as part of larger structures that embody them. This is necessary for the Workdlow package.

The keyResource string must be a unique  since this is used to index it in the hash table. 
when accessing a resource, the user must provide a partial object for wich the key can be obtained.
for example:

@data Person= Person{name, surname:: String, account :: Int ....)

keyResource Person n s ...= n++s@

the data being accesed must have the fields used by keyResource filled. For example

 @  readResource Person {name="John", surname= "Adams"}@

leaving the rest of the fields undefined
 
-}

-- | IResource has defaults definitions for all the methods except keyResource
-- Either one or other serializer must be defiened for default witeResource, readResource and delResource
class IResource a where 

        keyResource :: a -> String             -- ^ must be defined

        serialize :: a -> String                   -- ^  must be defined by the user
        serialize x= RS.runW $ tshowp x

        deserialize :: String -> a               -- ^  must be defined by the user
        deserialize str = RS.runR treadp str


        tshowp :: a -> RS.ST String                 -- ^  serializer in the 'RefSerialize' monad. Either one or other serializer must be defined to use default persistence
        tshowp x= do
           let str= serialize x
           let l= length str
           return $ show l  ++ " " ++ str

        treadp ::  RS.ST a                               -- ^ deserialize in the RefSerilzlize monad.
        treadp = do
           l <- RS.readp

           str <- replicateM l RS.anyChar
           return $ deserialize  str
           
        defPath :: a-> String       -- ^ additional extension for default file paths or key prefixes 
        defPath _ = "" 

    	-- get object content from the file 
    	-- (NOTE: reads and writes can't collide, so they-- Not really needed since no write is done while read
    	-- must be strict, not lazy )
    	readResource :: a-> IO (Maybe a)
        readResource = defaultReadResource
        
    	writeResource:: a-> IO()
        writeResource = defaultWriteResource
        
                   
    	delResource:: a-> IO()
    	delResource = defaultDelResource

defaultReadResource 	:: (IResource a) =>  a-> IO (Maybe a)
defaultReadResource x=  handle handler $ do     
             s <- readFileStrict  filename  :: IO String 
             return $ Just $ deserialize s                                                            -- `debug` ("read "++ filename)
             where
             filename=  defPath x++ keyResource x
             --handle :: IResource a => IOError -> IO (Maybe a)
             handler  (e :: IOError)
              |isAlreadyInUseError e = defaultReadResource x    -- maybe is being written. try again. 
                                                         
              | isDoesNotExistError e = return Nothing
              | isPermissionError e = error $ "readResource: no permissions for opening file: "++filename
              | otherwise= error $ "readResource: " ++ show e

defaultWriteResource :: (IResource a) => a-> IO()
defaultWriteResource x= safeWrite filename (serialize x)   --  `debug` ("write "++filename)
  where
  filename= defPath x ++ keyResource x

safeWrite filename str= handle  handler  $ writeFile filename str
     where          
     --handle :: a -> IOError -> IO ()
     handler  (e :: IOError)
       | isDoesNotExistError e=do 
                  createDirectoryIfMissing True $ take (1+(last $ elemIndices '/' filename)) filename   --maybe the path does not exist
                  safeWrite filename str               
     -- | isAlreadyInUseError e= writeResource x -- maybe is being read. try again
                                                   -- Not really needed since no write is done while read
       | otherwise =do
                hPutStrLn stderr $ "writeResource:  " ++ show e ++  " in file: " ++ filename ++ " retrying"
                safeWrite filename str
               {-
                               | isAlreadyExistsError   e =
                                              do
                                                   hPutStrLn stderr $ "writeResource: already exist file: " ++ filename ++ " retrying"
                                                   safeWrite path str



                               |   isAlreadyInUseError e =
                                              do
                                                   hPutStrLn stderr $ "writeResource: already in use: " ++ filename ++ " retrying"
                                                   safeWrite path str
                               |   isFullError   e =
                                              do
                                                   hPutStrLn stderr $ "writeResource: file full: " ++ filename ++ " retrying"
                                                   safeWrite path str
                               |   isEOFError  e =
                                              do
                                                   hPutStrLn stderr $ "writeResource: EOF in file: " ++ filename ++ " retrying"
                                                   safeWrite path str
                               |   isIllegalOperation   e=
                                              do
                                                   hPutStrLn stderr $ "writeResource: illegal Operation in file: " ++ filename ++ " retrying"
                                                   safeWrite path str
                               |   isPermissionError  e  =
                                              do
                                                   hPutStrLn stderr $ "writeResource:permission error in file: " ++ filename ++ " retrying"
                                                   safeWrite path str
                               |   isUserError   e  =
                                              do
                                                   hPutStrLn stderr $ "writeResource:user error in file: " ++ filename ++ " retrying"
                                                   safeWrite path str


                               | otherwise =do
                                                    hPutStrLn stderr $ "writeResource:   error  " ++ show e ++  " in file: " ++ filename 
                                                    safeWrite path str
                   -}

defaultDelResource :: (IResource a) => a -> IO()
defaultDelResource x=  handle (handler filename) $ removeFile filename  --`debug` ("delete "++filename)
	
             where
             filename= defPath x ++ keyResource x
             handler :: String -> IOError -> IO ()
             handler file e
               | isDoesNotExistError e= return ()
               | isAlreadyInUseError e= delResource x
               | isPermissionError e=    delResource x
   
               | otherwise = error ("delResource: " ++ show e ++ "for the file: "++ filename)

	
type AccessTime = Integer
type ModifTime    = Integer




-- | Resources data definition used by 'withSTMResources''     
data Resources a b
                   = Retry             -- ^ forces a retry
                   | Resources
                      { toAdd :: [a]    -- ^ resources to be inserted back in the cache
                      , toDelete :: [a] -- ^ resources to be deleted from the cache and from permanent storage
                      , toReturn :: b   -- ^ result to be returned
                      }


-- |  @resources= Resources  [] [] ()@
resources :: Resources a ()
resources= Resources  [] [] ()



-- Strict read from file, needed for default file persistence
readFileStrict f = openFile f ReadMode >>= \ h -> readIt h `finally` hClose h
  where
  readIt h= do
      s   <- hFileSize h
      let n= fromIntegral s
      str <- replicateM n (hGetChar h) 
      return str
    
      


{- | Key datatype can be used to read any object by the string key.

        @ data Key a=  Key  String a deriving Typeable @

         Example
         
        @  mst <- 'getResource' $ 'Key' 'keyofDesiredObject' (as :: DesiredDatatype)
             case mst of
               Nothing -> error $ \"not found \"++ key
               Just (Key k data) ->  return data

in cache retrieval the second parameter as a witness of the datatype desired to retrieve
The retrieval operation fill this second parameter with the real data
write of key objets in the cache has no sense, so writeXXX of a key object gives an error

However, writing and retrieving keys within larger objects are meaningful
so the serialize/deserialize  and tshowp/treadp works as expected, by writing/reading
string keys (not real objects)
@ serialize (Key "hello" undefined)  @
gives "hello"
-}


data Key a= Key String a deriving Typeable

-- | return the oject key with the second parameter undefined for object retrieval purposes
getKey :: (IResource a, Typeable a) => a -> Key a
getKey x= Key (keyResource x) x



instance IResource a => IResource (Key a) where
  keyResource (Key k _) = k
  serialize (Key k _)= show k
  deserialize k = Key (read k) undefined
  readResource key@(Key k typ) =handle handler $ do     
             s <- readFileStrict  filename  :: IO String
             return . Just . Key k $ deserialize s                                                            -- `debug` ("read "++ filename)
             where
             filename=  defPath typ ++ k
             --handle :: IResource a => IOError -> IO (Maybe a)
             handler  (e :: IOError)
              |isAlreadyInUseError e = readResource key    -- maybe is being written. try again. 
                                                         
              | isDoesNotExistError e = return Nothing
              | isPermissionError e = error $ "readResource: no permissions for opening file: "++filename
              | otherwise= error $ "readResource: " ++ show e ++ "for Key. "++ show k

  writeResource (Key _ x)= writeResource x

instance Show (Key  x) where
   show (Key k _) = show k

instance Read (Key  x) where
   readsPrec _ ('"':str) = [ (Key s undefined,s1)]
     where
     (s, r)= span (== '"') str
     s1= takeWhile isSpace $ tail r


