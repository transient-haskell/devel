module ConfDefs where

import Data.Array
import Data
import Data.Typeable
import Workflow

data Conf= Conf{
        syncCacheTime::Int,     
	sizeCache:: Int
     } 

     | ProjectConf
       	{groupWorkflow ::       Workflow  -- workflow de creación de grupo.
       	,catMajorities ::       [Majorities]
	,percentNewUser :: 	Int       
	--,checkApprobal :: 	Maybe (Subject -> Majorities -> Subject)
	} 
	deriving (Read,Show)


data Majorities= Majorities {      

	,percentAprobal:: 	Int
       	,percentNecessary :: 	Int
	,percentComplaint ::    Int
       	,votationTime :: 	Int
	,timeSpan :: 		Int

	}deriving (Read, Show)





