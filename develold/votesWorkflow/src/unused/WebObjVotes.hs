module WebObjVotes where

import Lang
import LangEs
import Data
import Vote
import UtilHTML(showChanges, change2HTML )
import WebServer
import Parameter
import WebObject as Wo
import Data.List
import Data.Maybe
import System.IO.Unsafe
import Control.Concurrent.MVar
import HSP


---------------------------webObject use for FreeChooser------------------

--diffLFields (LField _ value) (LField _ value')= diff value value'

diffForms lang (WForm _ fields)   (WForm _ fields')= concatMap diffo $ zip fields fields'
 				where diffo (f, f')= diff1 [showExpl lang f][showExpl lang f']

--diffFields (Field _ value)   (Field _ value')= [diff1 [value] [value']]

--showDiffLFields (LField _ value)(DField _ diff)= change2HTML  (strToList value) diff


showDiffForms lang f@(WForm n fields) f'@(WForm n' fields')= change2HTML (map (showExpl lang) fields)( diffForms lang f f')





constitutionalTimeSpan= "365"
 
pcap="percentApprobal"
pcam="percentAmend"
pcc="percentComplaint"
pcv="percentVotes"
vt="votationTime"
ts="timeSpan"
pnu="percentNewUser"
consti="Constitution"
ptype="propType"
pconst="Constitutional"
pord="Ordinary"
userf="userFields"
subf="SubjectFields"
params="parameters"






constitution= WForm consti
 [
   WForm ptype 
          [WForm pconst
                [Field pcap	"75" 
        	,Field pcam	"75" 
        	,Field pcc	"10" 
        	,Field pcv  	"75"
        	,Field vt	"30"
        	,Field ts	constitutionalTimeSpan
                ]
          ,WForm pord
                [Field pcap 	"50" 
        	,Field pcam 	"50" 
        	,Field pcc 	"20" 
        	,Field pcv	"30"
        	,Field vt	"10"
        	,Field ts	"0" 
                ]
          ,Field pnu "75"
          ]

  ,WForm userf []
  ,WForm subf []
  ,WForm params []
 
  ]
   
ord= "Ordinary"
cons= "Constitutional"
addConstHandlers =do
        addParam "explain" explain
        addParam "options" options
        addParam "valida" valida
   where
        options= Parameter  (\_-> VList [])
        
        valida= createFormula $ concatMap (++"\n")
            ["[name, value]-> f value"
            ," where"
            ," f=case name of"
            ," \"percentApprobal\" -> valida1"
            ," \"percentAmend\"    -> valida1"
            ," \"percentComplaint\"-> valida1"
            ," \"percentVotes\"    -> valida1"
            ," \"votationTime\"    -> valida2"
            ," \"timeSpan\"        -> valida2"
            ," \"percentNewUser\"  -> valida1"
            ," \"Ordinary\"        -> valida3"
            ," \"Constitutional\"  -> valida3"
            
        
            ," valida1 value= let val=cint value; error=VString \"Error: must be a number between 0 and 100\" in if val /= VNothing then  if val > (VInt 0) && val < (VInt 100) then VNothing else error else error"
        
            ," valida2 value= let val=cint value; error=VString \"Error: must be a number\" in if val == VNothing then error else VNothing"
        
            ," valida3 _= VNothing"
         ]
 
        explain= createFormula $ concat
          [
             "[VString name,VString lang] -> VString $ case name of\n"
         
             ,"         \"Ordinary\"-> case lang     of\n"
             ,"                               \"es\" -> LangEs.forOrdinary\n"
             ,"                               _ -> Lang.forOrdinary))\n"
             
             ,"         \"Constitutional\"-> case lang     of\n"
             ,"                               \"es\" -> LangEs.forConst\n"
             ,"                               _ -> Lang.forConst))\n"
         
             ,"         \"percentApprobal\"-> case lang     of\n"
             ,"                               \"es\" -> LangEs.agreepercentTxt\n"
             ,"                               _ -> Lang.agreepercentTxt))"   
             
             ,"         \"percentAmend\"-> case lang     of\n"
             ,"                               \"es\" -> LangEs.agreeAmendTxt\n"
             ,"                               _ -> Lang.agreeAmendTxt))\n"
        
             ,"         \"percentComplaint\"-> case lang     of\n"
             ,"                                    \"es\" -> LangEs.complaintTxt\n"
             ,"                                    _ -> Lang.complaintTxt))\n"
             
             ,"         \"percentVotes\"-> case lang     of\n"
             ,"                                    \"es\" -> LangEs.necessaryTxt\n"
             ,"                                    _ -> Lang.necessaryTxt))\n"
                
             ,"         \"VotationTime\"-> case lang     of\n"
             ,"                                    \"es\" -> LangEs.votationtimeTxt\n"
             ,"                                    _ -> Lang.votationtimeTxt))\n"
         
             ,"         \"percentNewUser\"-> case lang     of\n"
             ,"                                    \"es\" -> LangEs.newUserTxt\n"
             ,"                                    _ -> Lang.newUserTxt))\n"
             ,"         \"TimeSpan\"-> case lang     of\n"
             ,"                                    \"es\" -> LangEs.constTimeSpan\n"
             ,"                                    _ -> Lang.constTimeSpan))\n"
          ]
evalProperty env project name 
   | evalParam env pname == VNothing= loadParams project `seq`  undefined 
   | otherwise =  evalParam env pname
            where pname= project++"."++name
        
loadParams p=unsafePerformIO $ do 
   Rs s <-  justGetVResource $ Rs uSubject{sname=majorities,prosname= p}
   let SForm form= content s
   mapM addParam1 $ listParams  form
   where
   addParam1 (n,v) | isPrefixOf v "formula" = addParam n $ createFormula (drop 8 v)
                   | otherwise= return False
    
getProperty project name= getNProperty project [name] 

getNProperty project name=do	
	Rs s <-  justGetVResource $ Rs uSubject{sname=majorities,prosname= project} 
	return$  fromJust $ lookupNForm name $ form s
    where
	form s= f where SForm f= content s 
{-
getNProperty project name
   | lookupParam pname == Nothing= loadParams project `seq`  undefined
   | otherwise = v   
     where
        VString v= fromJust $ lookupParam pname 
	formFields s= fields where SForm (WForm _ fields)= content s
	pname= "project"++"."++name
-}
-- TODO  poner la gerarquia en nombres: pcap ..etc 
percentAprobal project category	    = do {s<-getNProperty project $ [consti, ptype,category,pcap];return $! read s}
percentReject project category	    = do {s<-getNProperty project $ [consti, ptype,category,pcc];return $! read s}
percentNecessary project category   = do {s<-getNProperty project $ [consti, ptype,category,pcv];return $! read s}
votationTime project category	    = do {s<-getNProperty project $ [consti, ptype,category,vt];return $! read s} 
timeSpan project category	    = do {s<-getNProperty project $ [consti, ptype,category,ts];return $! read s} 
percentNewUser project category	    = do {s<-getNProperty project $ [consti, ptype,category,pnu];return $! read s} 

getMajorities:: String -> String -> IO Majorities
getMajorities category project= do
	Just(Rs s) <-getVResource $ Rs uSubject{sname=majorities,prosname= project}
	return $! getSMajorities category project s


getSMajorities:: String -> String -> Subject -> Majorities
getSMajorities vcategory project s= values s 
  where    
	form= f where SForm f= content s
	values s= Majorities
	            {mcategory= category 
	            ,mproject=project
		    ,mapprobal=  (read . fromJust) $ lookupNForm [consti, ptype,category,pcap] form
		    ,mcomplaint= (read . fromJust) $ lookupNForm [consti, ptype,category,pcc] form
		    ,mnecessary= (read . fromJust) $ lookupNForm [consti, ptype,category,pcv] form
		    }
	category= show vcategory

data Comment= Comment{cTitle, cReplyTo, cFavor, cAuthor, cProject,cSubject:: String,
			cContent :: WebObject  } 

instance IWebObject  Comment where
    getName c = cTitle c

    wDisplay _ c= <p>
                    <p><b> <% cTitle c %></b> 
                       Support 
                       <b> <% cFavor c %></b>
                    </p>
		            <% wDisplay  "" $! cContent c %>
	              </p>

    edit lang _ c=  <p>
                     <p><input type="text" name="title"  value=(cTitle c) />
	                   <input type="hidden" name="replyTo" value=(cReplyTo c)/>
		               <input type="text" name="replyTo" value=(cReplyTo c)/> 
		               <select name="favor"> <br/><% map (\t -> <option><% t %></option>) $ stringOptions lang s %></select>
	                 </p>
	                 Wo.edit lang $! cContent c
	              </p>
	      where Just (Rs s)= unsafePerformIO $! getVResource $! Rs uSubject{sname=cSubject c, prosname=cProject c}

    getFromEnv  _ c env= 
      Comment{  cTitle=env->>"title",
			    cAuthor=env->>"cAuthor",
			    cReplyTo=env->>"replyTo",
			    cFavor=env->>"favor",
			    cProject=env->>"project",
			    cSubject=env->>"subject",
			    cContent=getFromEnv "" (LField "cContent" "") env
			 }