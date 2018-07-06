{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (span,splitAt,length)

--import DataCSVSemicolon

import System.Environment


import Debug.Trace
import Data.Text.Lazy as BS hiding (find)
import qualified Data.ByteString.Lazy as BSL

import Data.Text.Lazy.Encoding

import qualified Data.Map as M
import qualified Data.Vector as V
import System.IO.Unsafe
import Data.IORef

import System.Directory
import Data.Char

import Control.Monad


import Data.Monoid

main= do
   args <- getArgs
   let filename = args !! 0

   exc <- readFile "excluir.csv"
   let excluidos =  Prelude.map (BS.pack . Prelude.reverse . Prelude.tail . Prelude.reverse ) $ Prelude.lines exc
   putStrLn "sep=,"
   cabecera
   process excluidos filename







cabecera= do
  putStr "user id"
  comma
  putStr "idea"
  putStr "\r\n"

process excluidos filename= do

   contents <- return . decodeUtf8With (\_ _ -> return  '-') =<< BSL.readFile filename

   loop excluidos  contents

rresults= unsafePerformIO $ newIORef (M.empty :: M.Map Text  (Text,Text, V.Vector(Text,Text,Text)))

loop excluidos  str
    | str== mempty= return ()

    | otherwise= do
     let
       str1 = find "USER ID: " str
       (uid, rest)= span (/='\"') str1
       rest2= find "\",,\"1) &nbsp;"  rest

       (resp,rest3)= span (/='\r') rest2

     when (not $ uid `elem` excluidos) $ putLine uid resp

     loop excluidos  rest3

find _ "" = ""
find str str2=
   let (str3,str4) = BS.splitAt (BS.length str) str2
   in if str3== str then str4 else find str $ BS.tail str2

--findReplace _ _ "" = ""
findReplace a b s | BS.length s < BS.length a = s
                  | otherwise = if BS.isPrefixOf a s

                     -- then, write 'b' and replace jumping 'a' substring
                     then b <> findReplace a b (BS.drop (BS.length a) s)

                     -- then, write 'x' char and try to replace tail string
                     else BS.head s `BS.cons` (findReplace a b $ BS.tail s)




trans str=
           findReplace "&aacute;" "a" .
           findReplace "&eacute;" "e" .
           findReplace "&iacute;" "i" .
           findReplace "&oacute;" "o" $
           findReplace "&uacute;" "u" str

putLine uid resp= do
   BSL.putStr $encodeUtf8 uid ; comma ; BSL.putStr $ encodeUtf8 $ trans   resp
   BSL.putStr "\r\n"

comma= putChar ','
