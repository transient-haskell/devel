module Eval(mainc) where
import Include
import System.IO.Unsafe
import System.Eval.Haskell

{-
mainc1= do ef <- unsafeEval_ "\\f -> f [Int1 1,Int1 3]" ["Include"] [] []["."]  ::  IO (Either  [String]  (([Int1] -> Int1)-> Int1))
           return $ case ef of
             Right f1 ->  "hola "++ show (f1 sum1 :: Int1) 
             Left x   -> "error "++unlines x
-}
mainc= do  ef <- unsafeEval_ "sum2 [ A 1, A 2]" ["Include"] [] []["."]  ::  IO (Either  [String]  Int)
           return $ case ef of
             Right f1 ->  "hola "++ show f1 
             Left x   -> "error "++unlines x
