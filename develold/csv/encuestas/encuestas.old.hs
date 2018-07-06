{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (span,splitAt,length)

--import DataCSVSemicolon

import System.Environment
--import qualified Data.Map as M
--import qualified Data.Vector as V

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

main= do
   content <- getDirectoryContents "./"
   let files = Prelude.filter(isNumber . Prelude.head) content
   exc <- readFile "excluir.csv"
   let excluidos =  Prelude.map (BS.pack . Prelude.reverse . Prelude.tail . Prelude.reverse ) $ Prelude.lines exc
   mapM (process excluidos)files

   res <- readIORef rresults

   let resl= M.elems res
   putStrLn "sep=,"
   cabecera
   mapM_ putLine resl

cabecera= do
  putStr "user id"
  mapM etapa [1..10]
  putStr "\r\n"
  where
  etapa n= do
    comma >> putStr  "Numero Etapa"
    comma >> putStr "Te ha sido util esta etapa para trabajar tu idea de negocio?"
    comma >> putStr "Que es lo que MAS te ha ayudado de esta etapa? (Elige una)"
    comma >> putStr "Que cambios harias para mejorar esta etapa? "


process excluidos filename= do
   let etapa= read $ Prelude.take 2 filename
   contents <- return . decodeUtf8With (\_ _ -> return  '-') =<< BSL.readFile filename

   loop excluidos etapa contents

rresults= unsafePerformIO $ newIORef (M.empty :: M.Map Text  (Text,Text, V.Vector(Text,Text,Text)))

loop excluidos etapa str
    | str== mempty= return ()

    | otherwise= do
     let
       str1 = find "USER LOGIN: " str
       (username, rest)= span (/=';') str1
       rest1= find "USER ID: " rest
       (num, rest2)= span (/=';') rest1

       rest3= find ";;\"1) \160"  rest2
       (resp1,rest4)= span (/='\r') rest3
       rest5= find ";;\"1) \160\&"  rest4
       (resp2,rest6)= span (/='\r') rest5
       rest7= find ";;;\"1) "  rest6

       (resp3,rest8)= span (/='\r') rest7

     when (not $ num `elem` excluidos) $ add etapa username  num resp1 resp2 resp3

     loop excluidos etapa rest8


add etapa u n r1 r2 r3= do
   results <- readIORef rresults
   let muserdata = M.lookup n results

   writeIORef rresults $ case muserdata of
      Nothing -> M.insert n (u,n,V.replicate 12 ("","","") V.// [(etapa,(r1,r2,r3))]  ) results
      Just (u,n,vec) -> M.insert n (u,n,vec V.//[(etapa,(r1,r2,r3))]) results

putLine (u,n, vec)= do
--   putStr (show u) ; comma
   putStr (show n) ;
   mapM  printVec [1..10]
   putStr "\r\n"
 where
 printVec n= do
   comma;putStr (show n);
   let (r1,r2,r3)= vec V.! n
   comma; when (not $ BS.null r1) $ do quote; BSL.putStr (encodeUtf8 r1); quote
   comma; when (not $ BS.null r1) $ do quote; BSL.putStr (encodeUtf8 r2); quote
   comma; when (not $ BS.null r1) $ do quote; BSL.putStr (encodeUtf8 r3); quote

quote= BSL.putStr "\""


comma= putChar ','

find _ "" = ""
find str str2=
   let (str3,str4) = BS.splitAt (BS.length str) str2
   in if str3== str then str4 else find str $ BS.tail str2


