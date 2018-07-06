module Eval(mainc) where
import ConfDefs
import System.IO.Unsafe
import System.Eval.Haskell

mainc= do  i <- unsafeEval_ "head [1,2]" [] [] []["."] ::  IO (Either  [String]  Int)
           return $ show i 
    
    
    
