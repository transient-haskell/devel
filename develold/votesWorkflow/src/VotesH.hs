module VotesH where

import System.Directory(createDirectory)
import Control.Exception --(handle,Exception,throwIO)

import Data
import Vote
import VCache
import Aprobal(getMajorities1)
import WorkflowElem(execWorkflow)
import Control.Concurrent(forkIO)

import Lang(majorities,errorform,doesnotexist,grnamenotavail)

--subjectHasErrors sub@Subject{content= SForm form}= hasErr form 
--subjectHasErrors Subject{content= ConfText s}= hasErrorConfig s
--subjectHasErrors sub= Nothing

modifySubject sub= do
  let maj= getMajorities1 sub
  modify maj

  where
    modify v =do
             handleJust errorCalls  errorHandle $! do
	        withVResource (Rs sub) process `debug` "modify"
	         
	        return Nothing `debug` "modifySubject end"
     where
	        
        process Nothing= error  doesnotexist
        process _      = Rs sub

        errorHandle s= return$ Just s


createSubject sub=do
  let maj= getMajorities1 sub
  r <-  create maj
  return r `debug` "createSubject return"
  where

    create v= do
            syncVCache `debug` (" syncronizing, creating subject: "++ show sub) -- to create the project directory if not written
	    withVResources [Rp uProject{pname=project}, Rs sub] $ doIt
	    forkIO $ do{execWorkflow (Rs sub);return ()}  
	    print "createSubject end"
	    return Nothing 
	    
      where
 
        doIt  [Nothing, _,_]= error doesnotexist
        doIt  [Just (Rp pr),Nothing]= 
		[Rp pr{psubjects= addOne (sname sub)  (psubjects pr)}
		,Rs  sub 
		]   --addSubjectProc lastv CheckApprobalProp sub mrt]
		
        doIt _ = error grnamenotavail  

    
    project= prosname sub
    subject= sname sub
    ccategory= category sub
    scategory= show ccategory


createAmend amd= handleJust errorCalls (\s-> return $ Just s) $!do
    withVResources[Ra amd, Rs amd, uRl{lname=lname}] process
    return Nothing
 where
    lname= listAmendsName amd
    process [Nothing,Just (Rs s),Nothing]= [Ra  $! diffAmend s amd, Rl lname [aname amd] ] 
    process [Nothing,Just (Rs s),Just ( Rl n v)]= [Ra  $! diffAmend s amd, Rl n (aname amd:v) ]
    process [Just(Ra amd),_,_] = error grnamenotavail 
    process _          = error doesnotexist
    

 
         
 
