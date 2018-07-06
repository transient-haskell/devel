module Main where
import TestLambda
import Data.Typeable

        
data A= A{ a ::Int, b :: Lambda StringString, c:: Int } deriving (Read,Show)

main= do
   let a= read "A{ a= 1, b=Lambda (StringString (\\i ->i++i)), c=1 }" :: A
   let Lambda _ (StringString f)= b a
   print $ f "1"
   print a
