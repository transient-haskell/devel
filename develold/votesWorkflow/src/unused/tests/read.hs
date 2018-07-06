{-# OPTIONS -i/home/magocoal/haskell/votes/Votes.netvibes.UWA/src #-}
module Main where
import Data

main= do
   str <- readFile  "subject.xml"
   print (read str :: Subject)
