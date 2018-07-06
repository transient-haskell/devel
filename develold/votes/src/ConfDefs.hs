module ConfDefs where

import Data.Array
import Data
import Data.Typeable

data Conf= Conf{
        syncCacheTime::Int,     
	sizeCache:: Int
     } 

     | ProjectConf
       	{categories :: 		[String]
       	,catMajorities ::          [Majorities]
	,percentNewUser :: 	Int       
	--,checkApprobal :: 	Maybe (Subject -> Majorities -> Subject)
	} 
	deriving (Read,Show)
{-
instance Show Conf where
  show (Conf a b)= "Conf{ syncCacheTime="++show a++"sizeCache="++show b++"}"
  show _= "ProjectConf"
-}
instance Typeable Conf where
  typeOf _= mkTyConApp (mkTyCon "ConfDefs.Conf") []

data Majorities= Majorities       
	{percentAprobal:: 	Int
       	
       	,percentNecessary :: 	Int
	,percentComplaint ::    Int
       	,votationTime :: 	Int
	,timeSpan :: 		Int
	}deriving (Read, Show)





