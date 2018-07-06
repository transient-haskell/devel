{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
module TestLambda where
import Data.Typeable
import System.Eval.Haskell
import System.IO.Unsafe
import Data.List (elemIndex,isPrefixOf)

data Lambda a= Lambda String a

instance  Show (Lambda a) where
   show (Lambda s _)= "Lambda "++ s
   
   

instance (Typeable   a) =>  Read (Lambda a) where
   readsPrec _ st=  [(Lambda str2 $ lambda , str3)] where
     lambda= case unsafePerformIO $ eval_ str2 ["TestLambda"] [] [][""] of
                 Right mv ->   
				case mv of
                                    Just v -> v
                                    Nothing -> error $ "Nothing compiling: "++ str2 
                 Left s   -> error $ "syntax error compiling: "++ str2 ++" : "++ unlines s
     str=  dropWhile (==' ') st
     str1= if "Lambda" `isPrefixOf` str then drop 6 str else error $ "read: no parse in Array: "++str 
     (str2,str3)= case elemIndex ',' str1 of
             Nothing -> (str1,"")
             Just i  -> splitAt i str1
   

data StringString= StringString {t:: String->String  }
instance Typeable  StringString where    
  typeOf _= mkTyConApp (mkTyCon "Lambda String->String") []    


   
   

