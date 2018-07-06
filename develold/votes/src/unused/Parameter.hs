{-# OPTIONS -fglasgow-exts #-}
module Parameter(parameters, evalParam,maybeEvalParam, PType(..)
                ,PValue(..), addParam,createFormula,cint, lookupParam,jLookupParam) where

import System.Eval.Haskell
import Data.Typeable

import Data.Char
import Data.List(findIndex,splitAt,find,nub,filter)
import Data.Array(Array)
import qualified Data.Map as M
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Exception


data PValue = VPList ParamList | VInt Int | VFloat Float | VString String |VBool Bool | VArray (Array Int Float)
            | VList[PValue] | VProj Project | VSub Subject | VMaj Majorities |  VNothing 
            

type Params= [PValue]

data PType = Parameter  (Params-> PValue) | Formula  String (Params -> PValue) 
data ParamList= MVar (M.Map [Char] PType)

-- for extractParams
-- import Text.ParserCombinators.Parsec
-- import qualified Text.ParserCombinators.Parsec.Token as P
-- import Text.ParserCombinators.Parsec.Expr
-- import Text.ParserCombinators.Parsec.Language


import Debug.Trace

import Data

debug a b= trace b a






instance Typeable PValue where
  typeOf _= mkTyConApp (mkTyCon "Parameter.PValue") []

instance Eq PValue where
  (==) (VInt a) (VInt b)= a==b
  (==) (VFloat a) (VFloat b)= a==b
  (==) (VString a)(VString b)= a==b
  (==) (VBool a) (VBool b)= a==b
  (==) VNothing VNothing = True
  (==) _ _ = False
  
instance Ord PValue where
   compare (VInt a) (VInt b)= compare a b
   compare (VFloat a) (VFloat b)= compare a b
   compare (VString a) (VString b)= compare a b
   compare a b = error $ "compare "++show a++" "++show b
   
instance Show PValue where
        show (VInt x)= "VInt"++show x
        show (VFloat x)= "VFloat"++show x
        show (VString x)= "VString"++ x
        show (VBool x)="VBool"++show x
        show (VArray x)= "VArray"++show x
        show (VList x)= "Vlist"++ show x
        show (VProj x)= "VProj"++ show x
        show (VSub x)= "VSub"++ show x
        show (VMaj x) = "VMaj"++ show x 
        show (VNothing) = "VNothing"
                   
   
cint (VString s)=
    case readsPrec 0 s of
      [(a,_)] -> VInt a
      _ -> VNothing

instance Num PValue where
   (+) (VInt a) (VInt b)= VInt (a+b)
   (+) (VFloat a) (VFloat b)= VFloat (a+b)

   (-) (VInt a) (VInt b)= VInt (a-b) 
   (-) (VFloat a) (VFloat b)= VFloat (a-b) 

   (*) (VInt a) (VInt b)= VInt (a*b)     
   (*) (VFloat a) (VFloat b)= VFloat (a*b)  

   abs (VInt a) = VInt (abs a)
   abs (VFloat a)= VFloat(abs a)

   signum (VInt a) = VInt (signum a)
   signum (VFloat a)= VFloat (signum a)
   
   fromInteger i = VInt (fromIntegral i)

call= evalParam
call'=evalParam'

evalParam':: MVar (M.Map [Char] PType) -> [PValue]-> String -> PValue
evalParam' params prms p=
  case lookupParam' params p  of
    Just p -> evalParameter prms  p
    Nothing -> error ("Parameter "++p++" not found")
    

evalParam :: [PValue]-> String -> PValue
evalParam = evalParam'  parameters   
    
    
maybeEvalParam prms  p=
  case lookupParam p  of
    Just p -> Just $ evalParameter prms  p
    Nothing -> Nothing


evalParameter prms (Formula  expr f) = f prms 
evalParameter prms (Parameter  f) = f prms 
  
  
createFormula expr= Formula  expr $! unsafePerformIO $! compile expr

{-	
evalExpr env expr= f parmVals where
  parms= extractParams expr
  parmVals= map (evalParam env) parms
  f= unsafePerformIO $ compile expr
-}
 

compile exp= do
    let exp2="(\\c  -> case c of{"++exp++";_  -> error \"Error: wrong number/type of parameters \"++ show c ++ \"in expression:\\n\"++exp}) :: ([PValue] -> PValue)"

    m_f <- eval exp2 ["Parameter"]
    case  m_f of
      Just f -> return f
      Nothing   -> error $ "syntax error compiling: "++ exp




parameters= unsafePerformIO $ newMVar $ M.fromList 
  [("nusers", Parameter  (\_->VInt 3))
  ,("nusers2", (createFormula "[nusers,nusers2]->nusers + nusers"))
  ,("nusers3", (createFormula "[nusers2,nusers]->nusers2 + nusers"))
  ]

-- los parametros tienen que ser por proyecto.
--    parametros de proyecto con prefijo de proyecto
--    parametros generales, sin prefijo.
--    inicialmente no dejar parametros

lookupParam' :: MVar (M.Map [Char] PType) -> String -> Maybe PType
lookupParam' params  p = unsafePerformIO $ lookupParam1 params p where
 lookupParam1 params p= do
        m <- readMVar params 
        return $ M.lookup  p m

jLookupParam params name= case lookupParam' params name of
        Just (Parameter f) -> f
        Just (Formula _ f)-> f
        Nothing -> error $ "lookupParam: "++name++" does not exist"

lookupParam= lookupParam' parameters
        
addParam n v=do
        m <- takeMVar parameters
        if isJust $ M.lookup n m 
          then do
                putMVar parameters m
                return False 
          else do
                let m'= M.insert n v m
                putMVar parameters m'
                return True

                
addFormula name strFunc= addParam name  $ createFormula strFunc

      
{-
addParamForm type1 name = addParam name (Parameter function) where 
  function=  case type1 of
        User    -> (\env ->UString $ unsafePerformIO $ do 
                                s<- justGetResource User{uname= env->>name}
                                return $ lookupSForm (uParams s) name)
        Project -> (\env ->UString $ unsafePerformIO $ do   
                                s<- justGetResource Project{uname= env->>name}
                                return $ lookupSForm (pParams s) name)
        Subject -> (\env ->UString $ unsafePerformIO $ do 
                                s<- justGetResource Subject{uname= env->>name,prosname=env->>"project"}
                                let SForm content= scontent s                         
                                return $ lookupSForm content name)


extractParams fml= nub.filter (\x->(isLower.head) x && (not $ x `elem` reservedWords )) $ words fml
   where
    reservedWords = ["if","then","else"]

-}
{-
extractParams:: String -> [String]
extractParams str=  case (parse extractParams1 "" str) of
                                Left err -> error $ show err
                                Right xs -> (filter (/="").nub) xs 
  where
        lexer  = P.makeTokenParser haskellDef
        --myDef=haskellDef{reservedNames="cint":reservedNames haskellDef}
                                        
        identifier1 = do{c<- lower;cs <- P.identifier lexer;return (c:cs)}

        extractParams1 :: Parser [String]
        extractParams1  = do{spaces; x<- factor;return x}
        
        factor=  P.squares lexer (P.commaSep lexer identifier1) 
-} 

