{-# OPTIONS -fglasgow-exts  #-}
module Data where
import Data.Array.IArray
import Data.Array -- .Diff
import Data.Array.MArray
import GHC.IOBase(IOArray)
import System.IO.Unsafe
import Data.List(isPrefixOf)
-- for Parameter
import Control.Concurrent.MVar(MVar)
import qualified Data.Map as M
import Data.Maybe(fromMaybe)



data CronOp=CloseProposal | CheckApprobalProp | OpenPropForVote deriving (Eq, Read, Show)


{-!for Status      derive : Haskell2Xml, Show, Eq, Read !-} 
{-!for Change      derive : Haskell2Xml, Show, Read, Eq !-} 

type Percent= Float
data Status = Draft | Processing | Approbed Percent | 
              Rejected Why |Closed Status | Voted Status deriving (Read,Show,Eq)
 
data Change= Change Int [String] [String] deriving (Read,Show,Eq)


{-!for SContent derive : Haskell2Xml, Show !-} 
data SContent= Str String | Changes [Change] | ConfText String deriving (Read, Show, Eq)

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
                {sname           :: String -- proposal name
                ,aname           :: String -- amend name, when applicable
                ,prosname        :: String -- Project name
                ,category        :: String -- 
                ,authors         :: [String]
                ,topics          :: [String] 
                ,sstatus         :: Status
                ,content         :: SContent                 -- content of the proposal (different content types)
                ,options         :: Options                  -- options to vote
                ,votes           :: Array  Int PriorIVote-- (representant priority, option voted) array
                ,sumVotes        :: Array Int Int       -- total votes per option
                ,lastVote        :: Integer             -- last time to vote in clockTime units
                ,daysBefore      :: Int                 -- starting time to vote in days before end.
          
                }deriving (Read,Show)




---- To permit deserialization of vote Diff arrays
--instance (Read e, MArray a e IO) => Read (IOToDiffArray a Int e) where
--   readsPrec _ st= [(unsafePerformIO $ newDiffArray bounds assoc,str3)] where
--     str=  dropWhile (==' ') st
--     str1= if "array " `isPrefixOf` str then s else error $ "read: no parse in Array: "++str where (_,s)= splitAt 6  str
--     [(bounds,str2)] =  readsPrec 1 str1 :: [((Int,Int),String)]
--     [(assoc,str3)]= readsPrec 1 str2 :: [([(Int,e)],String)]
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
                visible   :: Bool
              
                }deriving (Read,Show,Eq)


