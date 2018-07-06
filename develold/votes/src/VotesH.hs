module VotesH where

import System.Directory(createDirectory)
import Control.Exception --(handle,Exception,throwIO)

import Data
import Vote
import Aprobal(addSubjectProc)
import Conf(hasErrorConfig,votationTime)

import Lang(majorities,errorform,doesnotexist,grnamenotavail)

--subjectHasErrors sub@Subject{content= SForm form}= hasErr form 
subjectHasErrors Subject{content= ConfText s}= hasErrorConfig s
--subjectHasErrors sub= Nothing

modifySubject sub= 
  case (category sub == "Constitutional" && sname sub == majorities) of   
    True-> case subjectHasErrors sub `debug` "subjectHasErrors" of
     			Left err ->   return $ Just err  `debug`"error"
     			Right v  ->   modify v `debug` "modify"
    False -> modify undefined

  where
    modify v =do
             handleJust errorCalls  errorHandle $! do
	        withVResource (Rs sub) process `debug` "modify"
	         
	        return Nothing `debug` "modifySubject end"
     where
        daysb= votationTime v
	lastvote = case category sub of
                 "Constitutional" -> flastVote $ fromIntegral daysb 
		 _ -> lastVote sub
	        
        process Nothing= error  doesnotexist
        process _      = Rs sub{lastVote= lastvote,daysBefore= daysb}

        errorHandle s= return$ Just s


createSubject sub=do
  r <- case (category sub == "Constitutional" && sname sub == majorities) of   
    True-> case subjectHasErrors sub   of
     			Left err ->   error err --return $ Just err 
     			Right v   ->   create v
    False -> create undefined
  return r `debug` "createSubject return"
  where

    create v= do
            syncVCache `debug` (" syncronizing, creating subject: "++ show sub) -- to create the project directory if not written
	    withVResources [Rp uProject{pname=project}, Rs sub, uRt{tname=cronTab}] $ doIt  
	    print "createSubject end"
	    return Nothing 
	    
      where
 
        doIt  [Nothing, _,_]= error doesnotexist
        doIt  [Just (Rp pr),Nothing,mrt]= 
		[Rp pr{psubjects= addOne (sname sub)  (psubjects pr)}
		,Rs  sub{lastVote=lastv,daysBefore=daysb} `debug` ("lastv="++ show lastv++ " daysb="++ show daysb)
		,Rt cronTab []]   --addSubjectProc lastv CheckApprobalProp sub mrt]
		
        doIt _ = error grnamenotavail  
        daysb = votationTime v
        lastv = case category sub of
                 "Constitutional" -> flastVote $ fromIntegral daysb
		 _ -> lastVote sub

    
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
    

 
         
 
