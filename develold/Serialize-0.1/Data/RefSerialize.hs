{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances  -fallow-incoherent-instances#-}
module Data.RefSerialize 
(   module Data.Serialize.Parser  -- a subset of Parsec 
    ,RefSerialize(   
        gshowParser, 
        greadParser
        )
 
    ,rShow
    ,rRead   
)   
 where
import qualified Data.Map as M
import Data.Serialize
import Data.Serialize.Parser
--import Token

class RefSerialize  c  where

    
    gshowParser :: c -> ST String
    greadParser :: ST c

  
{-
#ifdef Axioms

    refSerializeAxioms: Axioms c
    refSerializeAxioms= axioms{
         unary=   [Axiom "reverse" 
                            (\x ->  let str= rShow x
                                        y = rRead xtr
                                    in  y== x)
                                       
                   AxioM "pointer equality"
                           (\x ->   let str= rShow [x,x]
                                        [y,z] = rRead str
                                    in varName y== varname z)
                  ]
#endif                                          
-}         
         
rShow ::  RefSerialize c => c -> String
rShow c= runW $ gshowParser c
    
rRead ::  RefSerialize c => String ->c
rRead str= runR greadParser  str

          

readVar :: Read a => ST a
readVar = ST(\(Stat(c,s,v)) -> let (x,c2,s',v') = deserialize c s v in Right(Stat(c2,s',v'),x))

runR:: ST a -> String ->  a
runR   (ST f) str= 
    let (struct, vars)= readContext "where " str 
    in  case   f (Stat(M.empty,struct,vars) ) of
          Right (Stat _, a) -> a
          Left (Error s) -> error s
  




  
showVar :: Show a => a -> ST String
showVar var= ST(\(Stat(c,s,v)) -> let (c1,x) = serialize c var  in Right(Stat(c1,s,v),x))


runW (ST f) = case  f (Stat(M.empty,"",""))  of
              Right (Stat (c,_,_), str) ->
                let show1 c= concatMap (\(n,(_,v))->"v"++ show n++"= "++v++"; ")  $ M.assocs c 
                    vars= show1  c
                    strContext= if null vars  then "" else  " where {"++vars ++ "}"
                    
                in  str ++ strContext
            
              Left (Error s) -> error s
      


 

               ---------------------------


instance (Show a, Read a) => RefSerialize a where
    gshowParser = showVar
    greadParser = readVar
                
              



instance RefSerialize String where
    gshowParser str= showVar str --return $ ('"':str)++"\""
    greadParser =  readVar -- stringLiteral


instance  RefSerialize a => RefSerialize [a] where
    gshowParser []= return "[]"
    gshowParser (x:xs)=  do
           s1<- gshowParser x
           sn<- mapM f xs
           return $ "["++ s1++ concat sn ++"]"
           where
           f x=do  str <- gshowParser (x:: a)
                   return $ ", "++str
          
         
    greadParser =  brackets $ commaSep $ greadParser
                   



                        


