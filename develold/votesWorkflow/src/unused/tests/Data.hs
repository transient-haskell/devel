{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
module Data where
import Data.Array.IArray
import Data.Array.Diff
import Data.Array.MArray
import GHC.IOBase(IOArray)
import System.IO.Unsafe
import Data.List(isPrefixOf,findIndex,isPrefixOf)
import Data.Char(isSpace)
-- for Parameter
import Control.Concurrent.MVar(MVar)
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import Control.Workflow
-- para Lambda permitir read y show de funciones
import Data.Typeable
import System.Eval.Haskell
import Control.Workflow
import Data.TCache.Dynamic(Cache,IDynamic)
import Debug.Trace

debug1 a b= trace b a

data CronOp=CloseProposal | CheckApprobalProp | OpenPropForVote deriving (Eq, Read, Show)


{-!for Status      derive : Haskell2Xml, Show, Eq, Read !-} 
{-!for Change      derive : Haskell2Xml, Show, Read, Eq !-} 

type Percent= Float
data Status = Draft | Processing | Approbed Percent | 
              Rejected Why |Closed Status | Voted Status deriving (Read,Show,Eq)
 
data Change= Change Int [String] [String] deriving (Read,Show,Eq)


{-!for SContent derive : Haskell2Xml, Show !-} 
data SContent= Str String | Changes [Change] | Res ResourceVote | ConfText String deriving (Read, Show)

{-
instance Show SContent where
  show (Str str)= "Str "++"\""++str++"\""
  show (ConfText str)= "ConfText "++"\""++str++"\""
  show (Changes chs)= "Changes "++ show chs
-}

{-!for Options         derive : Haskell2Xml, Show !-}
{-!for Option          derive : Haskell2Xml, Show !-}
{-!for Why             derive : Haskell2Xml, Show, Eq,Read !-}

data Options = Approbal | ChooseOptions String Int [Option] deriving (Read,Show,Eq)
data Option= Option String Status    deriving (Read,Show,Eq)
 
data Why = Unconstitutional Percent | NegativeVote Percent | NotEnoughVotes Percent 
                                deriving (Read,Show, Eq)

{-!for Subject     derive : Haskell2Xml, Show !-} 
data Subject= Subject
                {statIndex       :: Int
                ,actionParams    :: Action
                ,sname           :: String -- proposal name
                ,aname           :: String -- amend name, when applicable
                ,prosname        :: String -- Project name
                ,category        :: String -- 
                ,authors         :: [String]
                ,topics          :: [String] 
                ,sstatus         :: Status
                ,content         :: SContent                 -- content of the proposal (different content types)
                ,options         :: Options                  -- options to vote
                ,votes           :: DiffArray  Int PriorIVote-- (representant priority, option voted) array
                ,sumVotes        :: DiffUArray Int Int       -- total votes per option
                ,lastVote        :: Integer             -- absolute end time for the current action in clockTime units
                ,daysBefore      :: Int                 -- starting time for the current action in days before end.

                }deriving (Read,Show)




-- To permit deserialization of vote Diff arrays
instance (Read e, MArray a e IO) => Read (IOToDiffArray a Int e) where
   readsPrec _ st= [(unsafePerformIO $ newDiffArray bounds assoc,str3)] where
     str=  dropWhile (==' ') st
     str1= if "array " `isPrefixOf` str then s else error $ "read: no parse in Array: "++str where (_,s)= splitAt 6  str
     [(bounds,str2)] =  readsPrec 1 str1 :: [((Int,Int),String)]
     [(assoc,str3)]= readsPrec 1 str2 :: [([(Int,e)],String)]
{-
instance  Eq (IOToDiffArray a i e) where
    (==) a b= error $ "Array comparison" ++ "arg1= "++show a++" arg2= "++show b
-}
{-!for ObjectUser derive : Haskell2Xml, Show !-} -- stand alone comand syntax 
{-!for User       derive : Haskell2Xml, Show !-} -- stand alone comand syntax 

{- 
The user can delegate by topic or by subject. What happens when the user delegates by topic 
but after the votation revert to delegate by subject or to vote directly?. I must go to his subject 
delegate, go to all open subjects for which the delegate has voted, rest the vote in each one of them 
and finally do the new opetation. Subject delegation has precedence over topic delegation. direct vote 
has precedence over the other two, so whenever a vote or a by subject cange is made, 
is is neccesary to undo any other less precendent voting procedure. The delegation goes in cascade
subjectuser y topic user
-} 

data ObjectUser= ObjectUser{ 
                uObject         :: String, 
                delegated       :: [Int], 
                delegatedTo     :: String
                }deriving (Read,Show,Eq)

data User= User{
                name      :: String, 
                password  :: String,
                uindex    :: Int,
                uProjects :: [ObjectUser],      
                usubjects :: [ObjectUser], 
                utopics   :: [ObjectUser]
                
                }deriving (Read,Show,Eq)

--(priority,vote)
newtype Priority= Priority Int    deriving (Eq, Ord)
newtype IndexUser= IndexUser Int  deriving (Eq, Ord,Read, Show)
newtype IndexVote= IndexVote Int  deriving (Eq, Ord)
type PriorIUser = (Priority, IndexUser) 
type PriorIVote = (Priority, IndexVote)

-- to avoid unnecesary serialization verbosity
instance Read IndexVote where
  readsPrec _ str= [(IndexVote i,str1)] where [(i,str1)]= readsPrec 1 str
  
instance Read Priority where
  readsPrec _ str= [(Priority i,str1)] where [(i,str1)]= readsPrec 1 str

  
instance Show IndexVote where
  show (IndexVote i)= show i
  
instance Show Priority where
  show (Priority i)= show i

-- voting priorities
direct          = Priority 4
bySubject       = Priority 3
byTopic         = Priority 2
byProject       = Priority 1
noVoted         = Priority 0

{-!for Project     derive : Haskell2Xml ,Eq, Read, Show !-} 



data Project= Project{
                pname     :: String,
                pauthor   :: String,
                pdescrip  :: String, 
                ptopics   :: [String], 
                users     :: [String], 
                psubjects :: [String],
                public    :: Bool,
                visible   :: Bool,
                propTypes :: [(String, Lambda Workf)] 
                }deriving (Read,Show)

instance Eq Project where
  p == p' = pname p == pname p'
  
--------------------application Configuration--------
data Conf= Conf{
        syncCacheTime::Int,     
	sizeCache:: Int,
	defaultConstitution :: String,
	objPath :: String
     } deriving (Read, Show)



----------All togeter-------------

instance Show (MVar a) where
 show mv= error " writing MVar"

instance Read (MVar a) where
 readsPrec str= error "no read for MVar"
 
data ResourceVote=    Ru User | Rp Project | Rs Subject  | Ra  Subject 
                    -- | Rf WebObject 
                    | Rl {lname:: String, lcontent :: [String]}
                    -- | Rt {tname:: String,crontab::[(Integer,CronOp,[String])]} 
                    | Rc Conf
                    | Rf String Bool
        
                    deriving(Show,Read, Typeable)
 
 
 
 --------------------WorkFlow-------------------------
-- para permitir read y show de funciones
data Lambda a= Lambda String a


instance  Show (Lambda a) where
   show (Lambda s _)= "Lambda "++ s
   
   

instance (Typeable   a) =>  Read (Lambda a) where
   readsPrec _ st=  [(Lambda str2 $! lambda , str3)] where
     lambda=  unsafePerformIO $ do
                res<- eval_ str2 ["Control.Workflow", "Data.TCache.Dynamic","Data"] [] []["."] 
                case res of
                 Right mv -> case mv of
                                    Just v -> return v  `debug` "XXXXXXXX evaluated XXXXXXX"
                                    Nothing -> error $ "Nothing compiling: "++ str2 

                 Left s   -> error $ "syntax error compiling: "++ str2 ++" : "++ unlines s
     str=  dropWhile (isSpace) st
     str1= if "Lambda" `isPrefixOf` str then drop 6 str else error $ "read: no parse in Array: "++str 
     (str2,str3)=  getLambdaStr 0 [] str1 
            
     getLambdaStr 0 xs []= (reverse xs,[])
     getLambdaStr n xs []=  error $ show n++" parentesis not closed"

     getLambdaStr n xs (c:cs)= case(c, n) of
       
        (')',1)-> (reverse (')':xs),cs)
        (')',n)-> getLambdaStr (n-1) (')':xs) cs
        ('(',n)-> getLambdaStr (n+1) ('(':xs) cs
        (x  ,n)-> getLambdaStr n     (x:xs) cs
        
data Majorities= Majorities 
        {percentAprobal:: 	Int
       	,percentNecessary :: 	Int
	,percentComplaint ::    Int
       	,votationTime :: 	Int
	,timeSpan :: 		Int

	}deriving (Read, Show)



type Elem   = ResourceVote

newtype Time= Time Integer


--cada process retorna un indice que indica la acción siguente a seguir. en caso de retornar 0, se pasa a la siguente accion de workflow

type Context=  (Action -> Elem -> Workflow IO Elem, Cache IDynamic)

newtype Workf= Workf ( Context ->Elem -> Workflow IO Elem)
newtype Filter = Filter(Context ->Elem -> Bool)
newtype CheckApprobal= CheckApprobal (Context -> Subject  -> Subject)
newtype CheckAmends = CheckAmends (Context -> [Elem] -> [Elem])  
newtype ExecFunc= ExecFunc (Context -> Elem -> IO  Elem)

instance Typeable  Workf where    
  typeOf _= mkTyConApp (mkTyCon "Workflow") []   

instance Typeable  Filter where    
  typeOf _= mkTyConApp (mkTyCon "Filter") []   

instance Typeable  CheckApprobal where    
  typeOf _= mkTyConApp (mkTyCon "CheckApprobal") []   

instance Typeable  CheckAmends where    
  typeOf _= mkTyConApp (mkTyCon "CheckAmends") []   

instance Typeable  ExecFunc where    
  typeOf _= mkTyConApp (mkTyCon "ExecFunc") []   

data Group= Group String | Sender   deriving (Read, Show)

data Action= Action Act Follow | Process Proc  deriving (Read, Show)

type Follow = [(Lambda Filter, Lambda Workf)]

data Proc   = Remite Group  String --Workflowname   -- remite a un grupo una propuesta con una categoría
               --para Goto
            | Create Elem
--quizá seria  bueno añadir llamadas libres que generen un elemento de Data.hs: Subjects, Users o Groups: Exec (Elem -> IO Elem)
            | Exec (Lambda ExecFunc)
            deriving (Read, Show)

type DoAmend= Bool

data Act    = Propose
            | VoteDefault Majorities  DoAmend
            | Vote  (Lambda CheckApprobal)
            | AmendDefault
            | Amend (Lambda CheckAmends)
            | Execute Int                         -- timespan en días
            deriving (Read, Show)
 
