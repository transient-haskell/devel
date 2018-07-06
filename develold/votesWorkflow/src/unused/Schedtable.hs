module Sched where

import System.IO.Unsafe
import Control.Exception as Control 
import System.Time --(Month,toClockTime)


import Data.Maybe(fromJust, catMaybes)
import Debug.Trace


import HSP

import Data
import Vote --hiding addUser,addProject,getProject,addSubject,getSubject,addAmend,getAmend
import VotesH

import Pages
import Lang
import LangEs
import WebObject
import WebObjVotes
import Aprobal



sched1 env=
	  case (env) of
		(("op","vor")
		 :("email", em)
		 :("pass", pass)
		 :("pass2", pass2)
	         :("reg",_):_)		-> regAction env

		(("op","vor")
		 :("email", em)
		 :("pass", pass)
		 :("val",_):_)		-> valAction env

		(("op","pro"):_)	-> modifyProjectPage env [] ""

		(("op","chp"):_)	-> userPage env [("project", env->>"project", "/", "Wed, 31-Dec-2006 12:00:00 GMT")] ""

		(("op","cre"):_)	-> createPrAction env

		(("op","sub"):_)	-> modifySubjectPage env [] ""

		(("op","vsub"):_)	-> showVoteSubjectPage env [] ""

		(("op","suc"):_)	-> createSubjectAction env

		(("op","vote"):_)	-> voteAction env
		
		(("op","voteam"):_)	-> voteAmendAction env

		(("op","delegate"):_)   -> delegateAction env

		(("op","login"):_)	-> return $ registrationPage env  ""

		(("op","phy"):_)	-> return philosophyPage
			
		(("op","del"):_)	-> deleteAction env

		(("op","regval"):_)	-> return $ registrationPage env  ""

		_ 			-> userPage env [] ""
	  where
		email 		= env ->> "email"
				    



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

		_			    -> userPage env [] ""
		

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

voteAction env=	do
	[Rp pr,Ru us]<- justGetVResources [Rp uProject{pname=project}, Ru uUser{Data.name= email}] `debug` ("vote project="++project++" user="++email)
    	indexu	<- indexUser email project
    	Rs sub <- justGetVResource $! Rs uSubject{sname=subject,prosname=project} `debug` ("indexUser="++ show indexu)
    	let ivote= iVote vote (stringOptions lang sub)
	tvotes 	<- finalList indexu  ivote project (sub,votes sub) us
	
	withVResource (Rs uSubject{sname=subject,prosname=project}) (votepr  pr us indexu tvotes ivote)
	reorderProposalAmends project subject 
	showVoteSubjectPage env [] tvotesuccess 
   where
        votepr :: Project-> User-> Int-> [PriorIUser] -> Int -> Maybe ResourceVote -> ResourceVote
	votepr  pr us indexu tvotes ivote (Just (Rs sub)) =Rs sub{ votes=lvotes, sumVotes=lsumVotes} where
		(lvotes,lsumVotes)=voteProc ivote sub pr indexu tvotes

	lang   = env->>acceptLang
	email  = env->>"email"
	project= env->>"project"
	subject= env->>"subject"
	vote   = env->>"option"
	
	tvotesuccess= case env->>acceptLang of
			"es" -> LangEs.votesuccess
			_    -> Lang.votesuccess

voteAmendAction env= do
	[Just (Rp pr),Just (Ru us)]	<- getVResources [Rp uProject{pname=project}, Ru uUser{Data.name= email}]
    	indexu	<- indexUser email project
    	
    	Just (Ra a) <- getVResource (Ra  uSubject{aname= aname,sname=subject,prosname=project}) 
	let ivote= iVote vote $ amendOptions lang	
	tvotes 	<- finalList indexu  ivote project ( a,votes a) us
	
	withVResource (Ra  uSubject{aname= aname,sname=subject,prosname=project})
			(votepr pr us indexu tvotes ivote) 
	syncVCache
	reorderProposalAmends project subject
	showVoteSubjectPage env [] tvotesuccess
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
	
	tvotesuccess= case env->>acceptLang of
			"es" -> LangEs.votesuccess
			_    -> Lang.votesuccess


createSubjectAction env= do
  if length ssname < 10 || types=="amend" && length aname < 10 then modifySubjectPage1 env [] (sub 0) tpropmorethan

   else do
	daysBefore<-	votationTime pname category 	--read $ env->>"daysBefore" ::Int

	r <- case types of	
	     "create"  -> createSubject $! sub daysBefore 
	     "modify"  -> modifySubject $! sub daysBefore
	     "amend"   -> createAmend  (sub daysBefore){aname=aname,votes=undefined} 
	case r of
	    Just msg  -> modifySubjectPage1 env [] (sub 0)$ 
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

	sub daysb	= 	Subject 
	                         {sname=ssname 
	                         ,aname= ""
				 ,prosname= pname 
				 ,category= category 
				 ,authors= sauthors 
				 ,topics=stopics 
				 ,sstatus=sstatus 
				 ,content=content 
	                         ,sumVotes=undefined
				 ,options= options 
				 ,votes= undefined 
				 ,lastVote= flastVote  daysb 
				 ,daysBefore=fromIntegral  daysb 
				 
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
	content	= 	if contentType=="text" then Str (env->>"content") else SForm (getFromEnv "" constitution env)
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




flastVote  daysb= 	tnow+daysb*secsDay 

createPrAction env 
    | length dname < 10 = modifyProjectPage1 env [] project tmorethan 

    | otherwise= do
 
	case typep of
		"modify" -> do  withVResource (Rp project) (\p-> Rp project)
				userPage env [] tsuccmodgroup


		_        -> do 	r<-projectRegister project
				case r of
					True->	do	
					    processNames dname lusers
					    createSubject maj
 					    modifySubjectPage1 (("type","modify"):("backTo","userPage"):env) [("project", dname, "/", "Wed, 31-Dec-2006 12:00:00 GMT")] 
 						maj  $ tsucccreagroup ++ tgroupmajorities
					    --userPage env [("project", dname, "/", "Wed, 31-Dec-2006 12:00:00 GMT")] tsucccreagroup

				 	False->	modifyProjectPage1 env [] project tgrnamenotavail

  	   
 where
	project = Project  dname pauthor pdescrip ltopics lusers lsubjects isPublic isVisibl 
	daysBefore= read $ fromJust $ lookupForm  "ConstitutionalVotationTime" $ fields constitution -- XXX 
	maj= Subject majorities "" dname  "Constitutional" [pauthor] [] Processing (SForm constitution) 
						Approbal undefined undefined (flastVote  daysBefore) (fromIntegral daysBefore) 
						
	  
	typep    = env->>"type"
	dname   = env->>"name"
	pauthor  = env->>"email"
	pdescrip= env->>"pdescrip"
	ptopics  = env->>"topics" 
	
	users   = env->>"users"
	subjects= env->>"subjects"

	isPublic= case (env->>"ispublic")of{ ""->False;_ ->True}
	isVisibl= case (env->>"isvisible")of{ ""->False;_ ->True}
	email= env->>"email"

	ltopics = strToList ptopics
	lusers  = addOne pauthor $ strToList users
	lusersu = if elem email lusers then lusers else email:lusers
	lsubjects= strToList subjects

	(tgroupmajorities,tgrnamenotavail, tmorethan, tsuccmodgroup, tsucccreagroup)= case env->>acceptLang of
				"es"-> (LangEs.groupmajorities, LangEs.grnamenotavail, LangEs.morethan, LangEs.succmodgroup, LangEs.succcreagroup)
				_   -> (Lang.groupmajorities, Lang.grnamenotavail, Lang.morethan, Lang.succmodgroup, Lang.succcreagroup)



valAction env= do
	v  <- userValidate email pass 

	case v of
		 True	-> backTo env

		 False	-> return $ validationPage env tloginfail 
    where
	pass  = env->>"pass"
	email = env->>"email"
	tloginfail=case env->>acceptLang of
			    "es" -> LangEs.loginfail
			    _    -> Lang.loginfail
	 	
regAction env = 
    if pass2 /= pass then 
	return $ registrationPage env "The password verification failed"
    else do
	r <- userRegister email pass
	case r of
		True	-> backTo env
		False	-> return $ registrationPage env temaregistered
    where
	pass 	= env->>"pass" 	
	pass2	= env->>"pass2"	
	email	= env->>"email"
	temaregistered = case env->>acceptLang of
			    "es" -> LangEs.emaregistered
			    _    -> Lang.emaregistered
    
backTo env= case backto of
				"modifyProject"  -> modifyProjectPage env cookies ""
				"modifySubject"	 -> modifySubjectPage env cookies ""
				_		 -> userPage env cookies tsuccreg

	where 	cookies	= [("email", email, "/", "Wed, 31-Dec-2006 12:00:00 GMT")]
		backto	= env->>"backto"
		email   = env->>"email"
		tsuccreg= case env->>acceptLang of
			    "es" -> LangEs.succreg
			    _    -> Lang.succreg
