{-# OPTIONS -XTypeSynonymInstances #-}
module Main where
import Data.IResource
import Data.IDynamic
import Data.Typeable

instance IResource Int where     
               keyResource x=  "I"
               serialize = show
               deserialize = read
               defPath _= "saved/"

instance IResource String where
               keyResource x=  take 5 x
               serialize = show
               deserialize = read
               defPath _= "saved/"
               
main= do
              putStrLn "see the code to know the meaning of he results"
              registerType :: IO Int           -- register both datatypes (Int, and String)
              registerType :: IO String
              let x= 1 :: Int

              let list= [IDynamic x, IDynamic "hello, how are you"]

              let assoc= zip (map keyResource list) list
              print $ lookup (keyResource (5 ::Int)) assoc

              mapM writeResource list
              mds <-  readResource $  IDynamic  "hello"
              case mds of
                Nothing -> error "must have been Just!"
                Just ds -> do
                             putStrLn $ serialize ds
                             let str= fromIDyn  ds ::   String
                             putStrLn str

                             let y=  fromIDyn  ds ::   Int   -- casting error
                             print y

