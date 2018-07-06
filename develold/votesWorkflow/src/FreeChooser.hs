{-# OPTIONS -fglasgow-exts #-}
module FreeChooser where

import System.IO.Unsafe
import Control.Exception as Control 
import System.Time --(Month,toClockTime)


import Data.Maybe(fromJust, catMaybes)
import Data.List(isPrefixOf)
import Debug.Trace
--import Control.Concurrent
	
import GHC.Conc

import HSP


import Data
import Vote
import VCache 
import VotesH
import HTTPParser2((->>),CgiOut(..))
import Pages
import Lang
import LangEs

import Aprobal
import Init
import CGI
import UtilHTML (amendOptions)
import Aprobal(getMajorities)
import Data.Array.Diff(newDiffArray)
import Data.Array.MArray(newArray_)

cgi= CGI cgi1

cgi1 env= do 
         
         initialize1   `debug` "in FreeChooser.hs"
         
         --id <-genid 
         --let env'= (transaction, id):env
         logEntry $ show env
         r <- sched env
         --if env->>"op" /= "vote" then logEntry $ id++" processed"else return ()
         return ( show r)  --`debug` "vuelve"
         where
         {-
	 genid= do
	      TOD a b <- getClockTime
	      return $ show a++show b
         -}
sched env=do 
     case env  of
          (("project", _)
            :("subject", _):_) -> showVoteSubjectPage env [] ""
            
          (("op","vor")
		    :("email", em)
		    :("pass", pass)
		    :("pass2", pass2)
		    :("reg",_):_)		-> regAction env em pass pass2
  
          (("op","vor")
		    :("email", em)
		    :("pass", pass)
		    :("val",_):_)		-> valAction env em pass
	  _ ->	    
	      case (env->>"op") of

		"pro"	-> modifyProjectPage env [] ""

		"chp"	-> userPage env [("project", env->>"project", "/", endCookie)] ""

		"cre"	-> createPrAction env

		"sub"	-> modifySubjectPage env [] ""

		"vsub"	-> showVoteSubjectPage env [] ""

		"suc"	-> createSubjectAction env  -- `debug` "createSubjectAction"

		"vote"	-> voteAction env
		
		"voteam"	-> voteAmendAction env

		"delegate"   -> delegateAction env

		"login"	-> return $ registrationPage env  ""

		"phy"	-> return philosophyPage
			
		"del"	-> deleteAction env

		"regval"	-> return $ registrationPage env  ""

		_ 			-> userPage env [] ""
				    



deleteAction ((op,_):("op2",op2):env)=do
	Just (Rp pr)<- getVResource $! Rp uProject{pname= project}
	if email `elem` users pr then
		case op2 of
			"pr" -> do	deleteProject project
					userPage env [] $ project++tsuccessdel

			"sub"-> do	deleteSubject project subject
					userPage env [] $ subject++tsuccessdel
	   else
		userPage env [] tyounotperm

	where
		email	= env->>"email"
		project	= env->>"project"
		subject = env->>"subject"
		(tsuccessdel, tyounotperm)= case env->>acceptLang of
					    "es"-> (LangEs.successdel, LangEs.younotperm)
					    _   -> (Lang.successdel, Lang.younotperm)



delegateAction env= do
	case (findChange env) of	
		("project",project,delegate)->
			if delegate /= tnewemail 
				then do changeDelegate lang email project (uProjects,"uProjects") delegate project
					userPage env [] ("Delegate for project :\""++project++"\" assigned to: "++delegate)
				else 	userPage env [] ""

		("topic",topic,delegate)    ->	
			if delegate /= tnewemail
				then do	changeDelegate lang email topic (utopics,"utopics") delegate project
					userPage  env [] ("Delegate for topic :\""++topic++"\" assigned to: "++delegate)
				else 	userPage env [] ""

		("subject",subject,delegate)->  
			if delegate /= tnewemail 
				then do	changeDelegate lang email (project++"/"++subject) (usubjects,"usubjects") delegate project
					userPage  env [] ("Delegate for subject :"++subject++" assigned to: "++delegate)
				else 	userPage env [] ""

		_	-> userPage env [] ""
		

   where
	lang= env->>acceptLang
	email= env->>"email"
	project= env->>"project"
	findChange [] = ("notfound","","")
	findChange env=
	     case(take 2 env) of
 
		[("type", typec) 
		 ,(what, delegate)]    		-> (typec,what,delegate)
	
		_ 				-> findChange (drop 1 env)
 
	tnewemail= case env->>acceptLang of
			    "es" -> LangEs.newemail
			    _    -> Lang.newemail

data QResult= Done | Queued
{-
--------queue calls that implies heavy withResource transactions like votation etc -----
type Env=[(String,String)]
data Task= Task (Env->IO()) Env
voteFIFO= unsafePerformIO.atomically $ newTChan ::Task 
isEmptyQueue= atomically $ isEmptyTChan voteFIFO
writeQueue f env= atomically $ writeTChan voteFIFO $ Task f env
readQueue= atomically $ readTChan voteFIFO



queueCall :: (Env->IO())->Env->QResult
queueCall f env=do
    queue
    v <-isEmptyQueue
    case v of
      True -> do
              f env
              return Done
      False-> do
              queue
              return Queued
    where
    queue= do
	    initialize
	    writeQueue f env
	    where
	    initialize= if not.initialized then  initialized else return True
	    initialized= forkIO sched >>= return True
	    sched= handle err $ do
		     Task f env <- readQueue
		     f env
		     sched

            err e= logEntry $ "error:"++show e
-}

queueCall f env=do
   f env
   return Done
--------------------------------------queueCall-----------------------------------------

voteAction env= do
    v<- queueCall voteAction1 env
    case v of
      Done  -> showVoteSubjectPage env [] tvotesuccess 
      Queued-> showVoteSubjectPage env [] "Your vote is being processed Please wait"

    where
    tvotesuccess= case env->>acceptLang of
			"es" -> LangEs.votesuccess
			_    -> Lang.votesuccess

    voteAction1 env= do
	[Rp pr,Ru us]<- justGetVResources [Rp uProject{pname=project}, Ru uUser{Data.name= email}] `debug` ("vote project="++project++" user="++email)
    	indexu	<- indexUser email project
    	Rs sub <- justGetVResource $! Rs uSubject{sname=subject,prosname=project} `debug` ("indexUser="++ show indexu)
    	let ivote= iVote vote (stringOptions lang sub)
	tvotes 	<- representedVotes indexu  ivote project (sub,votes sub) us
	
	withVResource (Rs uSubject{sname=subject,prosname=project}) (votepr  pr us indexu tvotes ivote)
	reorderProposalAmends project subject 
        logEntry $ (env->>transaction) ++"done"

	return()
        where
        votepr :: Project-> User-> Int-> [PriorIUser] -> Int -> Maybe ResourceVote -> ResourceVote
	votepr  pr us indexu tvotes ivote (Just (Rs sub)) =Rs sub{ votes=lvotes, sumVotes=lsumVotes} where
		(lvotes,lsumVotes)=voteProc ivote sub pr indexu tvotes `debug` ("voteProc "++"ivote="++show ivote++
		                                                         " tvotes="++show tvotes++ "sub="++show sub)

	lang   = env->>acceptLang
	email  = env->>"email"
	project= env->>"project"
	subject= env->>"subject"
	vote   = env->>"option"
	
voteAmendAction env= do
    v<- queueCall voteAmendAction1 env
    case v of
      Done  -> showVoteSubjectPage env [] tvotesuccess 
      Queued-> showVoteSubjectPage env [] "Your vote is being processed Please wait"

    where
    tvotesuccess= case env->>acceptLang of
			"es" -> LangEs.votesuccess
			_    -> Lang.votesuccess

    voteAmendAction1 env= do
	[Just (Rp pr),Just (Ru us)]	<- getVResources [Rp uProject{pname=project}, Ru uUser{Data.name= email}]
    	indexu	<- indexUser email project
    	
    	Just (Ra a) <- getVResource (Ra  uSubject{aname= aname,sname=subject,prosname=project}) 
	let ivote= iVote vote $ amendOptions lang	
	tvotes 	<- representedVotes indexu  ivote project ( a,votes a) us
	
	withVResource (Ra  uSubject{aname= aname,sname=subject,prosname=project})
			(votepr pr us indexu tvotes ivote) 
	syncVCache
	reorderProposalAmends project subject
	logEntry $ (env->>transaction) ++"done"
        return()

        where
	votepr pr us indexu tvotes ivote (Just(Ra  a))= Ra  a{ votes=lvotes, sumVotes=lsumVotes} where
 	    (lvotes,lsumVotes)= voteProc  ivote a    pr indexu tvotes
 	            
	lang   = env->>acceptLang
	aname  = env->>"aname"
	email  = env->>"email"
	project= env->>"project"
	subject= env->>"subject"
	vote   = env->>"option"

	strip name= takeWhile(/='.') $ tail name
	

--emptyVotes= {-unsafePerformIO $ newDiffArray -} read "array (0,0) [( 0,( 0, 0))]"  -- (representant priority, option voted) array
--emptySum= {-unsafePerformIO $ newDiffArray -} read "array (0,0) [(0,0)]"   -- total votes per option

createSubjectAction env= do
  {-if length ssname < 10 || types=="amend" && length aname < 10 then modifySubjectPage1 env [] (sub 0) tpropmorethan

   else do-}
        
                     
                      
	r <- case types of	
	     "create"  -> createSubject sub `debug` ( "createSubject, subject= ")
	     "modify"  -> modifySubject sub  `debug` "modifySubject"
	     "amend"   -> createAmend  sub {aname=aname} 
	case r of
	    Just msg  -> modifySubjectPage1 env [] sub $ 
      			    if msg == Lang.doesnotexist then  tdoesnotexist 
      			    else if msg == Lang.errorform then terrorform
      			    else if msg == Lang.grnamenotavail then tgrnamenotavail
      				 else msg
      				
	    Nothing   ->do	case(options ,types)of
				    (Approbal, "create") -> voteAction  ynenv
				    (Approbal, "amend")-> voteAmendAction ynenv
				    _		       -> showVoteSubjectPage nenv   cookies tproposalsubmit
				case backTo  of
				    "userPage" -> userPage env cookies tsucccreagroup
				    _ -> showVoteSubjectPage nenv   cookies tproposalsubmit
    where
	nenv= ("subject",ssname):env
	ynenv=  ("option",tyes):nenv
	cookies= [("project",pname,"/", "Wed, 31-Dec-2061 12:00:00 GMT"),
		  ("subject", ssname, "/", "Wed, 31-Dec-2061 12:00:00 GMT")]	

	backTo = env ->>"backTo"


	types		=	env ->> "type"

	oldname		=	env ->> "oldname"


	sub 	= 	blankSubject 
	                         {sname=ssname 
	                         ,aname= ""
				 ,prosname= pname 
				 ,category= category 
				 ,authors= sauthors 
				 ,topics=stopics 
				 ,sstatus=sstatus 
				 ,content=content 
	                         
				 ,options= options 
			         --,votes= emptyVotes
			         ,sumVotes= voteSlotsSum sub
				 ,lastVote  = 0   --  flastVote $ fromIntegral daysb 
				 ,daysBefore= 0   --  daysb 
				 
				 }
				 

				 

	aname	=	env->>"aname"
	pname	= 	env->>"project"
	category=	env->>"category"
	ssname	= 	env->>"name"
	sauthors=	[env->>"authors"]
	stopics	= 	catMaybes $ map getTopic env 
	sstatus	= 	if null status then Processing else read status
	status  =	env->>"status"
	contentType=	env->>"contentType" 
	content	= 	if contentType=="text" then Str (env->>"content") else  ConfText (env->>"content")  -- SForm (getFromEnv "" constitution env)
	typeOption=	env->>"typeOption"
	question=	(env->>"question")
	noptions=	read (env->>"noptions")::Int
	options	= 	case typeOption of
			    "approbal" -> Approbal
			    "choose"   -> ChooseOptions question noptions (map (\s-> Option s Processing) (lines1 (env->>"options")++[tcomplaint]))
	
	
{-
	year	=	read $ env ->>"year"
	month	=	read $ env ->>"month"
	day	=	read $ env ->>"day"
	hour	=	read $ env ->>"hour"
	

	clockt	=	toClockTime(CalendarTime year month day hour 
				0 0 0 Monday 0 timezone 0 True )
-}

	timezone=	env ->>"TZ"


	getTopic (k,v)= case (isPrefixOf "topic" k) of {True -> Just $ drop 5 k; _ -> Nothing}
	tcomplaint= case env->>acceptLang of
			"es" -> LangEs.complaint
			_    -> Lang.complaint
			
	(tyes,tdoesnotexist,tgrnamenotavail,terrorform, tsucccreagroup, tpropmorethan,tproposalsubmit, tsamenamegroup)= case env->>acceptLang of
		"es" -> (LangEs.yes,LangEs.doesnotexist,LangEs.grnamenotavail,LangEs.errorform, LangEs.succcreagroup,LangEs.propmorethan, LangEs.proposalsubmit, LangEs.samenamegroup)
		_    -> (Lang.yes,LangEs.doesnotexist,LangEs.grnamenotavail,Lang.errorform, Lang.succcreagroup, Lang.propmorethan,   Lang.proposalsubmit, Lang.samenamegroup)





createPrAction env 
    {- | length dname < 10 = modifyProjectPage1 env [] project tmorethan 

       | otherwise-}
    
    = do
 
	case typep of
		"modify" -> do  addVResource $  Rp pr
				userPage env [] tsuccmodgroup

 
		_        -> do 	handleJust errorCalls handle   $do
		                r <- projectRegister pr
				case r of
					True->	do	
					    processNames dname lusers
					    userPage env [("project", dname, "/", endCookie)] tsucccreagroup

				 	False->	modifyProjectPage1 env [] pr tgrnamenotavail

  	   
 where
        handle e= modifyProjectPage1 env [] pr e
	pr = Project  dname pauthor pdescrip ltopics lusers lsubjects isPublic isVisibl propTypes1

	typep    = env->>"type"
	dname   = env->>"name"
	pauthor  = env->>"email"
	pdescrip= env->>"pdescrip"
	ptopics  = env->>"topics" 
	
	users   = env->>"users"
	subjects= env->>"subjects"

	isPublic= case (env->>"ispublic" )of{ ""->False;_ ->True}
	isVisibl= case (env->>"isvisible")of{ ""->False;_ ->True}
	email= env->>"email"
        propTypes1= unsafePerformIO $ Prelude.catch ( return $ read expr) handle 
	            where expr=   env ->>"propTypesStr"
	                  handle e= error $ "syntax error compiling: "++ expr ++" : "++ show e
	ltopics =  strToList ptopics
	                  
	lusers  = addOne pauthor $ strToList users
	lusersu = if elem email lusers then lusers else email:lusers
	lsubjects= strToList subjects

	(tgroupmajorities,tgrnamenotavail, tmorethan, tsuccmodgroup, tsucccreagroup)= case env->>acceptLang of
				"es"-> (LangEs.groupmajorities, LangEs.grnamenotavail, LangEs.morethan, LangEs.succmodgroup, LangEs.succcreagroup)
				_   -> (Lang.groupmajorities, Lang.grnamenotavail, Lang.morethan, Lang.succmodgroup, Lang.succcreagroup)



valAction env email pass= do
	v  <- userValidate email pass 
	case v of
		 True	-> backTo env
		 False	-> return $ validationPage env tloginfail 

    where
	tloginfail=case env->>acceptLang of
		    "es" -> LangEs.loginfail
		    _    -> Lang.loginfail


regAction env email pass pass2 = do
    if pass2 /= pass then 
	return $ registrationPage env "The password verification failed"
     else do
	r <- userRegister email pass 
	case r  of
		True	-> backTo env
		False	-> return $ registrationPage env temaregistered 
    where
	temaregistered = case env->>acceptLang of
			    "es" -> LangEs.emaregistered
			    _    -> Lang.emaregistered

backTo :: [(String,String)] -> IO (CgiOut (HSP XML))
backTo env=let 	cookies	= [("email", email, "/", endCookie)]
		backto	= env->>"backto"
		email   = env->>"email"
		tsuccreg= case env->>acceptLang of
			    "es" -> LangEs.succreg
			    _    -> Lang.succreg
			    
           in  case backto of
				"modifyProject"  -> modifyProjectPage env cookies ""
				"modifySubject"	 -> modifySubjectPage env cookies ""
				_		 -> userPage env cookies  tsuccreg 


