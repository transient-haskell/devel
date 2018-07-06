{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, OverloadedStrings #-}
module Data.TCache.AWS (
amazonSDBPersist,
amazonS3Persist,
) where

import Aws
import Aws.SimpleDb hiding (select)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy.Char8(toChunks,fromChunks,pack,unpack)
import Network

import Aws.S3
import Data.Conduit
import Network.HTTP.Conduit
import qualified Data.Conduit.List as CList
import Data.List as L hiding (delete)
import Data.Maybe
import System.IO.Unsafe
import Control.Exception
import Control.Monad
import Data.TCache.Defs
import Data.String


sdbCfg =  defServiceConfig

-- | Define a default persistence mechanism in Simple AWS.
-- . It uses the credentials using `baseConfiguration` from the package aws,
-- which read them from the environment or from a file.
-- See `baseConfiguration` doc. for further information
--
-- The second parameter specify either if the domain will be cleared or not
--
-- Here is an example of default persistence for a user defined data structure:
--
-- > import Data.TCache.DefaultPersistence
-- > import Data.TCache.AWS
-- >
-- > instance  Serializable MyData where
-- >  serialize  =  pack . show
-- >  deserialize=  read . unpack
-- >  setPersist =  const . Just $ amazonSDBPersist "mydomain" False
--
-- If you like to use Simple AWS for all your data structures by default:
--
-- > instance  Serializable MyData where
-- >  serialize  =  pack . show
-- >  deserialize=  read . unpack
-- >  setPersist =  Nothing
--
-- > setDefaultPersist  amazonSDBPersist

amazonSDBPersist :: T.Text -> Bool ->   Persist
amazonSDBPersist domain delete=  unsafePerformIO $ withSocketsDo $ do
     cfg <- baseConfiguration
     when delete $ simpleAws cfg sdbCfg (deleteDomain domain)  >> return()  -- delete the domain to start afresh
     simpleAws cfg sdbCfg $ createDomain domain
     return $ _amazonSDBPersist cfg domain


--setAmazonSDBPersist :: T.Text -> Bool -> IO ()
--setAmazonSDBPersist domain delete = withSocketsDo $ do
-- cfg <- baseConfiguration
-- when delete $ simpleAws cfg sdbCfg (deleteDomain domain) >> return()
-- simpleAws cfg sdbCfg $ createDomain domain
-- setDefaultPersist $ _amazonSDBPersist cfg domain

_amazonSDBPersist cfg domain = Persist{
   readByKey= \key -> withSocketsDo $ do
       r <- simpleAws cfg sdbCfg $ getAttributes (T.pack key) domain
       case r of
        GetAttributesResponse [ForAttribute _ text] -> return $ Just   $ fromChunks [encodeUtf8 text]
        _ -> return Nothing,

   write= \key str -> withSocketsDo $ do
       simpleAws cfg sdbCfg
                     $ putAttributes  (T.pack key)  [ForAttribute tdata (SetAttribute (T.concat $ map decodeUtf8 $ toChunks str) True)] domain
       return (),
   delete= \ key  -> withSocketsDo $ do
     simpleAws cfg sdbCfg $ deleteAttributes (T.pack key)  [ForAttribute tdata DeleteAttribute] domain
     return ()
     }

s3cfg = Aws.defServiceConfig :: S3Configuration Aws.NormalQuery
tdata=  "textdata"

deriving instance Show GetObjectResponse

instance Show (ResumableSource a b) where show _= "source"


-- | Define a default persistence mechanism in Amazon S3.
-- . It uses the credentials using `baseConfiguration` from the package aws,
-- which read them from the environment or from a file.
-- See `baseConfiguration` doc. for further information
--
-- Here is an example of default persistence for a user defined data structure:
--
-- > import Data.TCache.DefaultPersistence
-- > import Data.TCache.AWS
-- >
-- > instance  Serializable MyData where
-- >  serialize=  pack . show
-- >  deserialize=   read . unpack
-- >  setPersist =  const . Just amazonSDBPersist "mydomain" False
--
-- If you like to use Amazon S3 for all your data structures by default:
--
-- > instance  Serializable MyData where
-- >  serialize=  pack . show
-- >  deserialize=   read . unpack
-- >  setPersist =  Nothing
--
-- > setDefaultPersist  amazonSDBPersist


amazonS3Persist :: Bucket -> Persist
amazonS3Persist bucket= unsafePerformIO $  do
  cfg <- baseConfiguration
  return $ _amazonS3Persist cfg  bucket

--setAmazonS3Persist :: Bucket -> IO ()
--setAmazonS3Persist bucket = withSocketsDo $ do
--  cfg <- baseConfiguration
--  setDefaultPersist $ _amazonS3Persist cfg  bucket
  

_amazonS3Persist cfg  bucket= Persist{
   readByKey = \key -> (withSocketsDo $ withManager $ \mgr -> do
     mr <- do
               o@(GetObjectResponse hdr rsp) <- 
                          Aws.pureAws cfg s3cfg mgr
                            $ getObject
                              bucket
                              (fromString key)  -- !> key
               if omDeleteMarker hdr
                then return Nothing  -- !> "nothing"
                else fmap Just $ responseBody rsp $$+- CList.consume  -- !> "just"
     return $ fmap fromChunks mr)
    `Control.Exception.catch` (\(e :: SomeException) -> return Nothing),
   write = \key str -> do
        withSocketsDo $ withManager $ \mgr -> do

          Aws.pureAws cfg s3cfg mgr
            $ putObject
              bucket
              (fromString key)
              (RequestBodyLBS str)
          return(),

   delete = \key -> withSocketsDo $ withManager $ \mgr -> do
          Aws.pureAws cfg s3cfg mgr
            $ DeleteObject (fromString key) bucket
          return()


     }


