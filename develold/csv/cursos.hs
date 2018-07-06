

module Main where
import Text.ParserCombinators.Parsec

import Data.CSV

import System.Environment
import qualified Data.Map as M
import qualified Data.Vector as V

import Debug.Trace


main= do
   args <- getArgs
   contents <- readFile (args !! 0)
   exc <- readFile "excluir.csv"
   let excluidos =  map (reverse . tail . reverse ) $ Prelude.lines exc

   let mtable  = parse csvFile "" . Prelude.tail $ Prelude.dropWhile (/= '\n') contents
   case mtable of
     Left str -> print $ "not parsed: " ++ show str
     Right table -> do

           let grouped =  group excluidos table -- $ V.toList table

           let result'= Prelude.map (\(id,name,map1) ->
                   let list= M.elems map1

                   in [id,name,list !! 0, list !! 1,list !!2,list !! 3, list !! 4,
                         list!! 5, list !! 6, list !! 7, list !! 8, list !! 9, list !! 10,list !! 11])
                  grouped
           cabecera
           Prelude.putStrLn $ genCsvFile result' -- encode result'

   where
   cabecera= do
      putStrLn "sep=,"
      putStr "userid"
      comma; putStr "email"
      mapM_ etapa [0..11]
      putStr "\r\n"

   etapa i = do comma; putStr $ "Etapa "++ show i

   comma= putChar ','

   group excluidos table= Prelude.foldr  (sum excluidos) [] table :: [(String,String,M.Map Int String)]

   sum excluidos (userid:name:email:courseid:coursetitle
           :totalsteps:completedsteps:coursecompleted:_:_)
       rss=

      if(elem userid excluidos) then  rss else
       case rss of
         [] ->     [(userid,email, M.singleton (proc coursetitle) coursecompleted)]

         (rss@((userid',email',etapas):rs)) ->

               if userid'== userid
                   then (userid',email', M.insert (proc coursetitle) coursecompleted etapas) :rs
                   else (userid,email, M.singleton (proc coursetitle) coursecompleted) : rss

proc coursettl=     --Km.4 - RELACI‡N CON CLIENTES
      read $ take 2 $ drop 3 coursettl

