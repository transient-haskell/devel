{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances  #-}
module Data.Serialize where
import GHC.Prim
import GHC.Exts
import Unsafe.Coerce
import Data.List(isPrefixOf,insertBy,elem)
import Data.Char(isAlpha,isAlphaNum,isSpace,isUpper)
import qualified Data.Map as M
import System.Mem.StableName
import System.IO.Unsafe
import Control.Monad (MonadPlus(..))

import Debug.Trace

debug a b= trace b a



type MFun=  Char -- usafeCoherced to char to store simply the address of the function
type VarName = String
type ShowF= String
type Context =  M.Map Int (MFun,ShowF)




readContext pattern str= readContext1 "" str where
 readContext1 s str| null str = (s,"")
                   | pattern `isPrefixOf` str = (s,drop n str)
                   | otherwise=   readContext1 (s++[head str]) (tail str)
                    where n= length pattern



hasht x=  (hashStableName . unsafePerformIO . makeStableName) x
 
varName x= "v"++ (show . hasht) x



match x context = M.lookup  (varName x)  context 

matchr x context= M.lookup   x  context 

numVar :: String -> Int
numVar var= read $ tail var

matchRead :: Read a => Context -> String -> String -> (a, Context, String)
matchRead context var vars=
 case matchr  (numVar var) context of
     Just( addr,_ ) ->     (unsafeCoerce addr, context, vars)
     Nothing       ->  --search in vars
            let (_, rest)= readContext (var++"=") (tail vars) 
                ((addr, s):_)= case  readsPrec 1 rest of
                                 [] -> error $ "readSprec string \""++rest ++ "\" can not be read"
                                 any -> any
                                 
                context'= M.insert (numVar var) (unsafeCoerce addr,  "") context
            in (addr, context', vars) 



serialize :: Show a => Context -> a -> (Context,String)
serialize context a  =  addContext context a
     where
     addContext context x=
         let hash = hasht x
             varname= "v" ++ show hash 
         in
         case M.lookup  hash  context  of
           Nothing -> 
                          let context'= M.insert hash (unsafeCoerce x, show x) context
                          in  (context', varname)
                                
           Just _  -> (context,varname)
   
deserialize  :: Read a => Context ->String -> String -> (a, Context, String,String)
deserialize context struct1 vars=
      let struct=  dropWhile isSpace struct1 in
        if not $ isAlpha (head struct) then 
          let ((x,str2):_)= readsPrec 1 struct
          in (x,context,str2, vars)
        else 
           let (var, str2) = readAlphaNum struct
               readAlphaNum str= span (isAlphaNum) str
               (addr,cs, vars')= matchRead context var vars
           in  (addr,cs, dropWhile isSpace str2, vars')
         
         
data Error= Error String
data Stat= Stat (Context, String, String) 
        
