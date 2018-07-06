{-# OPTIONS -F -pgmFtrhsx -fglasgow-exts #-}
module Pages where
import Control.Exception
import Data.Maybe(fromJust, fromMaybe,catMaybes)
import Data.Array.Diff
import System.Time --(Month,toClockTime)
import System.Locale
import System.Directory
import System.IO.Unsafe
import Data.List(find, findIndex, isPrefixOf, dropWhile, (\\), lines,sortBy)
import Data.Array.IArray(Array, array,(//),(!),bounds,elems )
import Text.Printf
import Debug.Trace
import Init (getConf1)

--import WebServer(CgiOut(Content,mime,cookies,Location,Status))
import HSP
--import HSPClientside hiding (content)

import Data
import VCache
import Vote
import Help
import Lang
import LangEs
import UtilHTML

import HTTPParser2
import Aprobal


                        

pageBodyPr:: HSP XML ->  String -> HSP XML -> HSP XML  -> HSP XML
pageBodyPr tabs  header  rigthHtml body= 
    
     <table id="tablebody" >
        <tr><td><% tabs %></td></tr>
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
        <tr><td  > -- bgcolor='#9999FF'
              
         
                <p  id="intro"> <% header %>    </p>
             
            </td>
        </tr>
        <tr>
            -- <td valign="top"  ><%leftHtml%></td>  --bgcolor="#00CC66"
            <td id="bodytd" ><%body%></td>
            <td valign="top"><%rigthHtml%></td>
        </tr> --bgcolor='#CCFFFF' --bgcolor='#00CC66' 
     </table>



leftSide lang= 
     <p>
                <a href= (cgiURL++"?project=")><h4><% tyourgroups %></h4></a>
                <a href=cgiURL> <h4><% tproposals %></h4></a>
                <h4><% tdelegation %></h4>
                <% validateWidget lang %>
                <br/>
                <a href= (cgiURL++"?op=login")> <h4><% tlogout %> </h4></a>
        </p>
    where
        (tlogout, tyourgroups,tproposals, tdelegation)= case lang of
                        "es" -> (LangEs.logout, LangEs.yourgroups, LangEs.proposals, LangEs.delegation)
                        _    -> (Lang.logout, Lang.yourgroups,Lang.proposals, Lang.delegation)

rigthSide lang=
              <p>
                <h4><% thelp %> </h4>
                <h4><% twhoweare %> </h4>
                <a href=(cgiURL++"?op=phy")> <h4> <%Lang.philosophy%></h4> </a>
                <h4> <%tfeedback%> </h4>
                <h4> <%tchangelog%> </h4>
              </p>
    where (thelp, twhoweare, tfeedback, tchangelog)= case lang of
                            "es" -> (LangEs.help, LangEs.whoweare, LangEs.feedback, LangEs.changelog) 
                            _    -> (Lang.help, Lang.whoweare, Lang.feedback, Lang.changelog) 




 
                        
pageBody tabs lang body= pageBodyPr tabs tintro  (rigthSide lang) body
    where tintro= case lang of
                                    "es" -> LangEs.intro
                                    _    -> Lang.intro

philosophyPage = do
         let tabs = createTabs [freeChooserName] "Philosophy" "Groups:" ["Groups","last group","last proposal"]
         Content { mime= page "philosophy"  $ pageBody tabs "en" Help.philosophy, cookies=[]}
                 

errorPage e=
   Content {
            mime= page "Error"  $
                 <p>
                     <b> Sorry, an error has ocurred:   </b>
                     <% case e of
                         ErrorCall errorTxt  -> errorTxt 
                         _  -> "The error has been logged."
                     %>
                     <br/> 
                      <a href =cgiURL > click here to return to the home page </a>
                 </p>
            ,cookies=[]
           } 
   where
   tabs = createTabs [freeChooserName] "Error" "" []


modifyProjectPage env cookies msg= do
   if null email then return $ registrationPage (("backto","modifyProject"):env) "" else

    case types of
        "create" -> modifyProjectPage1 env cookies blankProject msg

        "modify" -> do  Just (Rp pr) <- getVResource (Rp uProject{pname= project})
                        if email `elem` users pr then modifyProjectPage1 env cookies pr msg 
                                else userPage env []  tyounotperm

   where
        types= env->>"type"
        project= env->> "project"
        email = env->> "email"
        tyounotperm= case env->>acceptLang of
                        "es" -> LangEs.younotperm
                        _    -> Lang.younotperm

page :: String  -> HSP XML -> HSP XML
page title  bodyhtml=
        <html>
                <head>
                    <title><% title %></title>
                    <link rel="stylesheet" type="text/css"   href="style.css" />      
                </head> 
                <body>
                    <% bodyhtml %> 
                </body>
        </html>


modifyProjectPage1 env cookies pr msg= do

   return Content {
        mime=page "Create/modify group"  $ pageBody tabs lang $ body pr

        ,cookies = cookies 
     }

  where
        tabs= createTabs tabs types "" []
                    where
                    tabs = case types of
                      "modify" -> [freeChooserName, project]
                      _        -> [freeChooserName]

        lang= env->>acceptLang 
        body pr= <p>
                    <h1><% "Editing group: "++ (pname pr) %> </h1>
                    <font color="FF0000" size="4"> <b><%[ <p><%l%></p> | l <- lines msg] %></b> </font>
                    <form action=(cgiURL++"?op=cre") method="post">
                        <input type="hidden" name="op" value="cre"/>
                        <input type="hidden" name="oldname" value=( if types=="modify" then  project else "")/>
                    <input type="hidden" name="type" value=types/>

                        <p align="justify"> <% tthispagecollect %> </p>
                        <%vsep%>
                        <%if types/="modify" then 
                          <p><b><% tpleaseentername %></b><% texplainname %>
                             <br/>
                             <p align="center"><input type="text" size="75" name="name" value=(pname pr)/></p>
                             <%vsep%>
                          </p>
                          else <input type="hidden" name="name" value=(pname pr)/>
                        %>
                        <p align="justify"><b><% tpleasedescrip %></b><% texplaindescrip %></p>
                        <center> 
                                <textarea name="pdescrip" rows="20" cols="65" warp="true"><% pdescrip pr %> </textarea> 
                        </center>
                        <%vsep%>
                        <p align="justify"><b><% tpleasetopics %> </b><% texplaintopics %> </p>
                        <p align="center"> 
                                <textarea name="topics" rows="3" cols="65" wrap="True"><% listByComma (ptopics pr) %> </textarea>
                        </p>
                        <%vsep%>
                        <p align="justify"><b><% tpleaseemails %></b><% texplainemails%></p>
                        <p align="center"> 
                           <textarea name="users" rows="10" cols="65" wrap="True" ><% unlines $ users pr %> </textarea>
                        </p>
                        <% vsep %>
                        <p>"this is the flow of proposals, Please modify it only if you know what you are doing"</p>
                        <center> 
                                <textarea name="propTypesStr" rows="20" cols="65" warp="true"><% propTypes1 %> </textarea> 
                        </center>
                        <input type="hidden" name="subjects"  value=(concatMap ("\n"++) $ psubjects pr)/>
                        <p align="justify"><input type="checkbox"  checked=checkPublic name="ispublic" value="OFF" /><b><% tpleasepublic %></b> <% texplainpublic %></p>
                        <p align="justify"><input type="checkbox" checked=checkVisible name="isvisible" value="OFF"/><b><% tpleasevisible %></b> <% texplainvisible %></p>
                  
                        <input type="submit" name="OK" value="OK"/><input type="reset" name="cancel" value="cancel"/>

                    </form>

                 </p>
        propTypes1 :: IO String
        propTypes1= case types of
                   "create"->  do
                                conf <-getConf1 
                                let def = defaultConstitution conf 
                                
                                readFile $ dataPath ++ def
                   "modify"-> return $ show (propTypes pr)
        checkPublic= show (public pr) 
        
        checkVisible= show (visible pr)

        types= env->>"type"
        project= env->> "project"

        (tpleasevisible, tpleasepublic, tpleaseemails, tpleasetopics, tpleasedescrip, tpleaseentername, tthispagecollect, texplainpublic, texplainvisible, texplaintopics, texplaindescrip,texplainemails,texplainname)= case env->>acceptLang of
                                "es" -> (LangEs.pleasevisible, LangEs.pleasepublic, LangEs.pleaseemails, LangEs.pleasetopics, LangEs.pleasedescrip, LangEs.pleaseentername, LangEs.thispagecollect, LangEs.explainpublic, LangEs.explainvisible, LangEs.explaintopics,LangEs.explaindescrip,LangEs.explainemails, LangEs.explainname)
                                _    -> (Lang.pleasevisible, Lang.pleasepublic, Lang.pleaseemails, Lang.pleasetopics, Lang.pleasedescrip, Lang.pleaseentername, Lang.thispagecollect, Lang.explainpublic, Lang.explainvisible, Lang.explaintopics,Lang.explaindescrip, Lang.explainemails, Lang.explainname)

modifySubjectPage env cookies  msg =
   if null email then return $ validationPage (("backto","modifySubject"):env) "" else

        do 
          Rp pr         <- justGetVResource (Rp uProject{pname= project}) 
           
          Just (Rs s)   <- case types  of
                                        "amend"         ->      do{Just(Rs s) <- getVResource $! Rs uSubject{sname=ssname,prosname=project};return $! Just $ Rs s{authors=[email]} }
                                        "modify"        ->      getVResource $! Rs uSubject{sname=ssname,prosname=project}
                                        "create"        ->      return $! Just $ Rs blankSubject{prosname=project,authors=[email],lastVote=tnow}
                                
          if email `elem` users pr then modifySubjectPage1 env cookies s msg 
                else userPage env []  tyounotperm

   where
        project         = env->>"project"
        ssname          = env->>"subject"
        types           = env->>"type"
        email           = env->>"email"
        tyounotperm     = case env ->>acceptLang of
                            "es" -> LangEs.younotperm
                            _    -> Lang.younotperm


modifySubjectPage1 env cookies s  msg =do 
   Rp pr <- justGetVResource $ Rp uProject{pname=prosname s} `debug` ( "project= "++ prosname s)
   let lcategories= map (\(n,_) -> n) $ propTypes pr -- `debug` (show conf)

   return Content{ mime= page "create/modify a Subject"  
      $ pageBody tabs lang $ body  pr  s year month day hour lcategories, cookies=cookies}

  where 
        tabs = createTabs tabs  (types ++ " proposal") "" []
          	      where
          	      
          	      tabs= case types of
          	            "create" -> [ freeChooserName,prosname s]
          	            _        -> [ freeChooserName,prosname s, sname s]
        body :: Project -> Subject -> Int -> Month -> Int -> Int -> [String] -> HSP XML 
        body pr  s year month day hour lcategories= 
          --boxid <- genId
          
           <p>
            <form action=cgiURL method="post"> 
                <input type="hidden" name="op" value="suc"/> 
                <input type="hidden" name="type" value=types/>
                <input type="hidden" name="backTo"  value=(env->>"backTo")/>
                <input type="hidden" name="project" value=(prosname s)/>
                <font color="FF0000" size="4"> <% msg %> </font>
                <h1><% case types of{"modify"-> teditingsub; "create"->tpropcreation;"amend"->"Amending: "} ++ (sname s) %></h1>
                
                <%vsep%>
                <% case types of
                        "create" -> -- edit name, category
                            <p><p align="justify"><b><% tpleasename %></b> <% texplainnamesub %> </p>
                                   <p align="center"><input type="textfield" size="80" name="name" value=""/></p>
                                   <%vsep%>
                                   <p align="justify"> <b><% tpleasecategory %> </b><%texplaincategory%> </p>
                                   <%[<p><input type="radio" checked=(show $ if x == category s then True else False) 
                                            name="category" value=(x)/> <b><% x %></b>
                                      </p> | x <- lcategories]%>  -- get categories dynamically
                            </p>

                        "amend"  -> -- edit amend name
                            <p><p align="justify"><b> <%tpleasemodifname%></b><% tnamewillappear %> </p>
                                   <p align="center"> <input type="text" size="80" name="aname"  value=""/></p> 
                                   <%vsep%>]
                                   <input type="hidden" name="name"value=(sname s)/>
                                   <input type="hidden" name="category" value=(show $ category s) />
                            </p>

                        "modify" -> -- no edit name, edit category
                            <p><input type="hidden" name="name" value=(sname s)/>
                                 <p><p align="justify"> <b> <%tpleasecategory %> </b><% texplaincategory %></p>
                                              <%[<p><input type="radio" checked=(show $ x==category s) 
                                                    name="category"  value=(show x)/>
                                                           <b><% show x %></b>
                                                    </p>
                                                        | x <- lcategories] 
                                                  %>
                                           </p>
                                  %>
                                </p>
                %>
                <input type="hidden" name="authors" value=(listByComma $ authors s)/>

		<p>
                                  <b><% tpleasetopicssub %></b>
                                  <p><% topicBoxes %></p>
                </p>
		      

                         



                <b><% tpleasecontent %></b>
                <br/>
                <p align="center"><% contents %></p> 
                <%vsep%>
                        
                
                
                
                <p>

                         <p align="justify">
                                < input type="radio" checked=checkApprobal  name="typeOption" value="approbal" {- onClick=(boxid # hideElem) -} /> --"document.getElementById('boxes').style.display='none'"/>
                                <b><% tthisisapprobal %></b>
                                <% texplainapprobal %>
                         </p>
 
                         <p align="justify">
                                < input type="radio"    name="typeOption" value="choose" {-onClic=(boxid # showElem) -}/> --"document.getElementById('boxes').style.display='block'"/>
                                <b><% tthisischoose %></b> 
                                <% texplainchoose %>
                         </p>
                        
                        
                    
                   <span {-id=boxid -} style="visibility: visible; position: relative" >
                        <table> 
                        <tr> <td width="5%" > </td><td></td></tr>
                        <tr> <td></td><td><b><% tpleasequestion %> </b></td></tr>
                        <tr> <td></td>
                                <td><p align="center"><textarea name="question" rows="4" cols="60" wrap="True"><% question lang s %> </textarea></p>
                                </td>
                        </tr>
        
                        <tr> <td></td>
                                <td>  <p align="justify"> <b> <%tpleaseoptions %></b>,<%texplainoptions %> </p> </td>
                        </tr>
                        <tr> <td></td>
                                <td>  <p align="center"> 
                                        <textarea name="options" rows="10" cols="60" wrap="True"><% if null doptions  then texampleoptions else doptions %></textarea>
                                        </p>
                                </td>
                        </tr>  
                        <tr> <td></td>
                                <td> <b><% tnoptionstochoose %></b>
                                <input type="text" name="noptions" value="1"/>
                                <% tmustbeless %>
                                </td>
                        </tr>
                        </table>
                  </span>
          
                  <%vsep%>
                </p>
                
                <%if sstatus s == Draft then 
                                <p align="justify">
                                   <%vsep%>
                                   <b><% tstatusofprop %></b>
                                   <%" "%>
                                   <select name="status">
                                        <option>Processing</option>
                                        <option>Draft</option>
                                   </select>
                                   <%" "%>
                                   <% texplainstatus %>
                                </p>
                   else <%vsep%>
                %>
                <p align="justify"><input type="submit" name="submit" value=tcreatmod />
                                   <input type="reset" name="Cancel" value=tcancel />
                </p>
           </form>
           </p>

                where
                        contents        = case (content s) of
                                                Str str    -> <span><input type="hidden" name="contentType" value="text"/>
                                                                                <textarea name="content" rows="30" cols="60" wrap="True"><%str%> </textarea></span>
                                                ConfText str-> <span><input type="hidden" name="contentType" value="Conf"/>
                                                                                <textarea name="content" rows="30" cols="60" wrap="True"><%str%> </textarea></span>

                                                --SForm form -> <p><input type="hidden" name="contentType" value="form"/><% Wo.edit lang "" form %></p>

                        doptions        = concatMap (++"\n") $ takeWhile (/= tcomplaint) $ stringOptions lang s 
                                            
                        topicBoxes      = map (\t -> <span><input type="checkbox" name=("topic"++t) value="OFF" checked=(show $ checked t)/><b><% t %>  </b></span>) (ptopics pr)

                                where
                                        checked t= elem t (Data.topics s) 

                        checkApprobal= case (Data.options s) of
                                         Approbal -> "true"
                                         _        -> "false"

                        checkChoose = case (Data.options s) of
                                         Approbal -> "false"
                                         _        -> "true"
                                         

        types           = env->>"type"
        CalendarTime year month day hour _ _ _ _ _ _ _ _        = toUTCTime (TOD t1 0)
                                                 where t1= lastVote s

        lang= env->>acceptLang
        (tpropcreation,tstatusofprop, tpleaseoptions, tpleasequestion, tthisischoose, tthisisapprobal, tpleasecontent, tpleasetopicssub, tpleasemodifname, tpleasename, tpleasecategory, tnoptionstochoose, tnamewillappear, tmustbeless, texplainstatus,texplainoptions,texplainnamesub,                                                        explainstatus,texplainchoose,texplaincategory,texplainapprobal,teditingsub,tcreatmod,tcancel,tcomplaint,texampleoptions)= 
            case lang of
                "es" -> (LangEs.propcreation, LangEs.statusofprop, LangEs.pleaseoptions, LangEs.pleasequestion, LangEs.thisischoose, LangEs.thisisapprobal, LangEs.pleasecontent, LangEs.pleasetopicssub, LangEs.pleasemodifname, LangEs.pleasename, LangEs.pleasecategory, LangEs.noptionstochoose, LangEs.namewillappear, LangEs.mustbeless, LangEs.explainstatus, LangEs.explainoptions, LangEs.explainnamesub, LangEs.explainstatus,LangEs.explainchoose, LangEs.explaincategory,LangEs.explainapprobal,LangEs.editingsub, LangEs.creatmod,LangEs.cancel,LangEs.complaint,  LangEs.exampleoptions)
                _    -> (Lang.propcreation, Lang.statusofprop, Lang.pleaseoptions, Lang.pleasequestion, Lang.thisischoose, Lang.thisisapprobal, Lang.pleasecontent, Lang.pleasetopicssub, Lang.pleasemodifname, Lang.pleasename, Lang.pleasecategory, Lang.noptionstochoose, Lang.namewillappear, Lang.mustbeless, Lang.explainstatus, Lang.explainoptions, Lang.explainnamesub, Lang.explainstatus, Lang.explainchoose, Lang.explaincategory,Lang.explainapprobal, Lang.editingsub, Lang.creatmod,Lang.cancel, Lang.complaint,    Lang.exampleoptions)



validateWidget lang= 
        <form action=cgiURL>    
                <b><% temailt %></b>
                <br/>
                <input type="hidden" name="op" value="vor"/>
                <input type="text" size="10" name="email" value=""/>
                <br/>
                <b><% tpasswordt %></b>
                <br/>
                <input type="password" size="10" name= "pass"/>
                <br/>
                <input type="submit" name="val" value= tvalidatet/>
                <br/>
                <a href=(cgiURL++"?op=regval")><b><% tregister %></b></a>  
        </form>
    where
            (temailt,tpasswordt,tvalidatet,tregister)= case lang of
                    "es" -> (LangEs.emailt, LangEs.passwordt,LangEs.validatet,LangEs.register)
                    _    -> (Lang.emailt, Lang.passwordt,Lang.validatet,Lang.register)


userPage env (cookies :: [(String,String,String,String)])  msg=  do
 --   page <- listProjects env msg
 --   return Content{mime=page, cookies= cookies
   let email= env->>"email"
   let project= env->>"project"
   let prCookie  
        | not $ null cookies = return cookies
        | null email = return cookies
        | not $ null project= return  [("project", project, "/", endCookie)]
        | otherwise= do
             prs <-  projectListNames  email
             return $ if not $ null prs then [("project", head prs, "/", endCookie)] else []
 
   projectCookie <- prCookie  
   --page <- if null project then listProjects env2 msg  else showProject env 
   page <- if null project then listProjects env msg  
           else do
                  mpr<- getVResource $ Rp uProject{pname= project}
                  case mpr of
                   Nothing      -> listProjects env msg
                   Just (Rp pr) -> showProject pr env  
   return Content{mime= page, cookies= (projectCookie++cookies)}

        


listProjects env msg= do
    pubpr <- publicProjects

    if email=="" then return $ page "freeChooser.com"  $ pageBody tabs lang $! notLogged pubpr
     else do
        prs  <- projectList email
        Just (Ru us)   <- getVResource (Ru uUser{Data.name= email})

        return $ page "freechooser.com"  $ pageBody tabs lang(body prs us pubpr)

        
   where
        tabs = createTabs [] Lang.freechooser tyuchus []
        lang= env->>acceptLang
        notLogged pubpr = 
           <p>
                
                <%vsep%>
                <a href=(cgiURL++"?op=pro&type=create")> <b> <%tcreateyourgroup %> </b></a>
                <%taskacreator%>
                        
                <%tyoupublic%>
                <%vsep%>
                <%if null pubpr then <br/> else
                        <p>
                          <table tableStyle> <% tpublicinvitedto %> 
                                <tr><th><% tnamet %></th><th><% tauthort %></th><th><% ttopicst %></th><th></th><th><% tactions %></th></tr>
                                
                                <% map (listProject lang blankUser) pubpr %>
                          </table>
                          <h3><% tnogroup %></h3>
                        </p>
                %>
           </p>

        body prs us pubpr =     
                        
         <p><p align="center"> <h1><% email ++ " " ++ tgroupspage %></h1></p>
                <font color="FF0000" size="4"><b><% msg %></b></font>
                <br/>
                <% tthissectionshows %>


                <% if null prs then  
                   <span>
                     ddsdfsadf<h3><% tyouarenotgroup %></h3>
                     <p>
                        <a href=(cgiURL++"?op=pro&type=create")> <b><% tcreateyourgroup %></b> </a>
                        <% taskacreator %>                       
                     </p>
                     <br/>
                     <p><% tyoupublic %></p>
                   </span>
                  else <br/>
                %>        
                <% listProjects %>
                <a href=(cgiURL++"?op=pro&type=create")><b><% tcreategroup %></b></a>
                <br/>
                <h3><% tnogroup %> </h3>
         </p>

           where
                        listProjects =  
                             <p>
                                <%if null prs then <br/> else
                                        <table tableStyle><caption><% tgroupsmemberof %> </caption> 
                                                <% header %> 
                                                <% map (listProject lang us) prs %>
                                        </table>
                                %>
                                <%
                                  if null pubs then <br/> else
                                        <table tableStyle><caption><%tpublicinvitedto%></caption> 
                                                <% header %> 
                                                <%map (listProject lang us) pubs%>
                                        </table>
                                %>
                             </p>
                        header= <tr><th><% tnamet %></th><th><% tauthort %></th><th><% ttopicst %></th><th><% tdelegate %></th><th><% tactions %></th></tr> 

                        pubs= pubpr\\ prs
                

        email= env->>"email"  
        project= env->>"project"
        
        (tyuchus,tstrListProjects,tyouarenotgroup, tthissectionshows, tpublicinvitedto, tyoupublic, tnogroup, tnamet, tgroupspage, tgroupsmemberof,tdelegate,tcreateyourgroup,tcreategroup,tauthort,ttopicst,tactions,taskacreator)= 
            case lang of
                    "es" -> (LangEs.yuchus,LangEs.strListProjects,LangEs.youarenotgroup, LangEs.thissectionshows, LangEs.publicinvitedto, LangEs.youpublic, LangEs.nogroup, LangEs.namet, LangEs.groupspage, LangEs.groupsmemberof,LangEs.delegate,LangEs.createyourgroup,LangEs.creategroup,LangEs.authort, LangEs.topicst,LangEs.actions,LangEs.askacreator)
                    _    -> (Lang.yuchus,Lang.strListProjects,Lang.youarenotgroup, Lang.thissectionshows, Lang.publicinvitedto, Lang.youpublic, Lang.nogroup, Lang.namet, Lang.groupspage, Lang.groupsmemberof, Lang.delegate,Lang.createyourgroup,Lang.creategroup,Lang.authort, Lang.topicst,Lang.actions, Lang.askacreator)


showProject pr env=        do
        TOD tnow _ <- getClockTime
        --Rp pr <- justGetVResource $! Rp uProject{pname= project}
        
        Ru us<-   if null email then return $! Ru blankUser 
                           else justGetVResource $! Ru uUser{Data.name=email}
        case( null email, elem email $ users pr, public pr)of
                (True,_,True)   -> showp us pr tnow
                (_,True, _)     -> showp us pr tnow
                (False,False,True)-> do addUserToProject us pr
                                        showp us pr tnow
                        
        
                (_,False,False) -> return $ <br/>
        content <- showp us pr tnow
        morep <- moreProjects email
        let tabs = createTabs [freeChooserName] project "Other projects:" morep
        return $ page project  $! pageBodyPr tabs (showTextContent $ pdescrip pr) (rigthSide lang) content 



  where

    project  = env->>"project"
    email    = env->>"email"


    lang= env->>acceptLang

    showp :: User -> Project -> Integer ->    IO (HSP XML)        
    showp us pr tnow =do
          jprs <- mapM (listSubject lang project email tnow ) $ zip (psubjects pr) ([1,2..]::[Int])
          prs  <- return $ catMaybes jprs
           
          return $ 
                
           <p>

                <b><% ttopicst++": " %>
                   <font color="#000080" size="4">
                     <b><% listByComma $ ptopics pr %> </b>
                   </font>
                </b>
                <br/>
                <%if pauthor pr /= email then <br/>
                   else
                      <a href=(cgiURL++"?op=pro&project="++project++"&type=modify") ><% teditgroup %></a>
                %> 
                <%vsep%>
                
                <%case(psubjects pr) of
                   []   -> <h2><% tnoproposals %></h2>
           
                   _    -> 
                          <p> <h2><% tsubmittedprop %></h2>  
                                         <table tableStyle> <caption><%tproposalspr %></caption> 
                                           <tr>
                                               <th><% tnamet %></th><th><%tauthort%></th><th><% ttopicst %></th>
                                               <th><%tstatust%></th><th><%tdelegate%></th><th><%tactions%></th>
                                           </tr>
                                           <% prs %>
                                     </table>
                                  </p>
                %>
                <a href= (cgiURL++"?op=sub&type=create")> <b> <%tsubmitnew%></b></a>
                <%vsep%>
                <p align="justify" > <b><% ttopicst %></b> <% texplaintopicslist %></p>

                <%if not $ null email then 
                        <form action=cgiURL> 
                                <input type="hidden" name="op" value="delegate" />
                                <table tableStyle> 
                                  <caption><% ttopicst %></caption> 
                                  <tr><th><% ttopic %></th><th><% tstrdelegated %></th><th><% tdelegatedto %></th><th><% tchangedel %></th></tr> 
                                  <% listTopics us pr %>
                                </table>        
                    </form>     
                  else <br/>                              
                %>
           </p>



    listTopics us pr=  map (showTopicUser us )(ptopics pr)

    showTopicUser us  topic =
                case(find (\ut-> (uObject ut)==topic) $ usertopics)of
                        Just ut -> <tr> <% topic %>
                                        <td><% show $ length $ delegated ut %> </td>
                                        <td><% delegatedTo ut %></td>
                                        <% row1 %>
                                        <% row2 %>
                                  </tr>

                        Nothing -> <tr> <td> <% topic %> </td>
                                        <td> <% tnone %> </td>
                                        <td> <% tnone %> </td>
                                        <% row1 %>
                                        <% row2 %>
                                   </tr>

                where   usertopics= utopics us
                        row1= <td><input type="hidden" name="type" value="topic"/><input onfocus="this.value=''" name=topic vaue=tnewdeluser/></td>
                        row2= <td><input type="submit" name="change" value= tchangebut /></td>

    (tstrproject, ttopic, tsubmitnew, tproposalspr, tsubmittedprop, tnoproposals, tnone, tnewdeluser, texplaintopicslist,teditgroup,tdontexistdel,tclickherelist,tchangebut, tstrdelegated, tdelegatedto, tchangedel,tnamet,tauthort, ttopicst, tstatust, tdelegate, tactions)
             = case env->>acceptLang of
                                "es" -> (LangEs.strproject, LangEs.topic, LangEs.submitnew, LangEs.proposalspr, LangEs.submittedprop, LangEs.noproposals, LangEs.none, LangEs.newdeluser, LangEs.explaintopicslist, LangEs.editgroup,LangEs.dontexistdel, LangEs.clickherelist, LangEs.changebut, LangEs.strdelegated, LangEs.delegatedto, LangEs.changedel
                                            ,LangEs.namet,LangEs.authort, LangEs.topicst, LangEs.statust, LangEs.delegate, LangEs.actions)
                                _    -> (Lang.strproject, Lang.topic, Lang.submitnew,Lang.proposalspr, Lang.submittedprop, Lang.noproposals, Lang.none, Lang.newdeluser, Lang.explaintopicslist,Lang.editgroup, Lang.dontexistdel,Lang.clickherelist, LangEs.changebut, Lang.strdelegated, Lang.delegatedto, Lang.changedel
                                            ,Lang.namet,Lang.authort, Lang.topicst, Lang.statust, Lang.delegate, Lang.actions)
                                
objectUserDel lang us name typeObject= case (objectUserDelegate us name typeObject) of
                                        Nothing ->tnewemail
                                        Just "" ->tnewemail
                                        Just del-> del
        where tnewemail=case lang of
                            "es" -> LangEs.newemail
                            _    -> Lang.newemail

listSubject lang project email tnow (subject,i) =do
        let cl= if i `rem` 2 ==0 then "oddRow" else "evenRow"
        Rs s  <- justGetVResource $ Rs uSubject{prosname= project, sname=  subject}
        if sstatus s == Draft && authors s /=[email] then return Nothing 
         else do
          Just (Ru us) <-if null email then return $! Just $! Ru $ blankUser else getVResource $! Ru uUser{Data.name= email}
  
          return.Just  $
                <tr class=(cl)><td><%sname s %></td><td><p><%[<a href= ("mailto:"++auth)><%auth++" "%></a>| auth<-authors s]%></p></td>
                    <td><% listByComma ( Data.topics s)%></td>
                    <td><% showFinalStatus lang $ sstatus s %></td>
                    <td>
                      <%if not $ null email then 
                                <form action=cgiURL method="post">
                                    <input type="hidden" name="op" value="delegate"/>
                                    <input type="hidden" name="type" value="subject"/>
                                    <input type="text" onfocus="this.value=''" name=(subject) value=( objectUserDel lang us subject usubjects) />
                                    <input type="submit" name="change" value="change"/> 
                                </form> 
                            else <span/>  %>
                    </td>
                    <td>
                      <%if canEdit email s (rangev s)
                                then 
                                <p style="margin-top: 0; margin-bottom: 0">
                                                <% editLink $ sname s %>   <% delLink project subject %>
                                                <% viewVoteLink $ sname s %>
                                        </p>
                                else <% viewVoteLink $ sname s %>
                          %>
                    </td>
                   

                </tr>
        where
                showFinalStatus lang Draft = case lang of 
                                                "es" -> <span><% LangEs.draft %></span>
                                                _    -> <span><% Lang.draft %></span>
                showFinalStatus lang Processing = case lang of 
                                                "es" -> <span><%  LangEs.processing %></span>
                                                _    -> <span><% Lang.processing %></span>
                                                
                showFinalStatus lang (Closed st)
                            | lang=="es"= <p> <% LangEs.tclosed %> <% showFinalStatus lang st %> </p>
                            | otherwise = <p> <% Lang.tclosed %> <% showFinalStatus lang st %> </p>

                showFinalStatus lang (Approbed pc) 
                            | lang=="es" = explain LangEs.approbed LangEs.explApprobed pc
                            | otherwise  = explain Lang.approbed Lang.explApprobed pc

                                                
                showFinalStatus lang (Rejected (Unconstitutional pc))
                            | lang=="es" = explain LangEs.rejected LangEs.explUnconst pc
                            | otherwise  = explain Lang.rejected Lang.explUnconst pc
  
                                                                           
                showFinalStatus lang (Rejected (NegativeVote pc))
                            | lang=="es" = explain LangEs.rejected LangEs.explNegativeVote pc
                            | otherwise  = explain Lang.rejected Lang.explNegativeVote pc
 
                showFinalStatus lang (Rejected (NotEnoughVotes pc))
                            | lang=="es" = explain LangEs.rejected LangEs.explNotEnoughVotes pc
                            | otherwise  = explain Lang.rejected Lang.explNotEnoughVotes pc

                explain rej explanation pc=  <p><% rej %><a href=("\"javascript:alert(\'"++explanation++"\')\"") >
                                                            <% show pc ++" %" %>
                                                         </a>
                                             </p>

                                                   
                editLink name           =       <a href= (cgiURL++"?op=sub&project="++project++"&subject="++name++"&type=modify")><% tedit %> </a>
                delLink p s             =       <a href= (cgiURL++"?op=del&op2=sub&project="++p++"&subject="++s)><% tdelete %> </a>
                viewVoteLink name       =       <a href= (cgiURL++"?op=vsub&project="++project++"&subject="++name)><% tview %> </a>
                rangev s                =       rangeForVote s tnow

                (tdelete,tedit, tview)= case lang of
                        "es" -> (LangEs.delemailt, LangEs.edit, LangEs.view)
                        _    -> (Lang.delemailt,Lang.edit, Lang.view)

canEdit email s rangev = rangev < daysBefore s && rangev >0 && [email] == (authors s) && sstatus s == Draft


        

listProject lang us pr=
        
                <tr>    
                        <td><%nameLink%></td><td><%authorLink%></td>
                        <td><% listByComma  (ptopics pr) %></td>
                        <td><%if Data.name us `elem` users pr then 
                                        <form action=cgiURL> 
                                                <input type="hidden" name="op" value="delegate"/>
                                                <input type="hidden" name="type" value="project"/>
                                                <input type="text" onfocus="this.value=''" name=(pname pr) value=( objectUserDel lang us (pname pr) uProjects)/>
                                                <input type="submit" name="change" value="change" /> 
                                        </form> 
                               else <br/>
                             %>
                        </td>
                        <td>
                          <%if Data.name us==authord then 
                                <p style="margin-top: 0; margin-bottom: 0">
                                        <% editLink %> <%delLink %>
                                </p>
                             else <span/>
                          %>
                           
                          <% viewLink %>
                        </td>
                </tr>
        where
        
                editLink        =       <a href= (cgiURL++"?op=pro&project="++namep++"&type=modify")><% tedit %> </a> :: HSP XML
                delLink         =       <a href= (cgiURL++"?op=del&op2=pr&project="++namep)><% tdelete %></a> :: HSP XML
                viewLink        =       <a href= (cgiURL++"?project="++namep)><% tselect %> </a>  :: HSP XML
                nameLink        =       <a href= (cgiURL++"?project="++namep)><% namep %></a> :: HSP XML
                authorLink      =       <a href= ("mailto:"++authord)><% authord %></a>  :: HSP XML
                namep           =       pname pr
                authord         =       pauthor pr
                (tdelete,tedit,tselect)         =       case lang of
                                            "es" -> (LangEs.delete, LangEs.edit, LangEs.select)
                                            _    -> (Lang.delete, Lang.edit, Lang.select)


anchors lang amends= 
  <p>
        <p>
           <a href= "#vote"> <b><% tvote %></b></a>
           <b><% " / " %></b>
           <a href= "#delegate"> 
              <b><% tdelegate %></b>
           </a>
        </p>
        <%if null amends then <nothing/>
           else <p>
                                 <a href= "#amendments"> <b><% tamendments %></b></a>:<br/>:
                                 <ul> 
                                     <%[<li>
                                                        <a href=("#"++a)> 
                                                                        <b><% a %></b>
                                                        </a>,
                                                        <a href= ("#"++a++"vote")> 
                                                                        <b><% tvote %></b>
                                                        </a>
                                        </li>| a <- amends]
                                     %>
                                 </ul>
            </p>
    %>
  </p>
    where
        (tamendments,tvote, tdelegate)= case lang of
            "es" -> (LangEs.amendments,LangEs.vote, LangEs.delegate)
            _    -> (Lang.amendments,Lang.vote, Lang.delegate)

showSubject lang s rangev email =
        <p>
                <p align="center"> 
                        <b><% tvvodel %></b>
                        <br/>
                        <font color="#000080" size="6"><% sname s %></font>
                </p>
                <br/>
                <% showSubjectHeader lang s email %>
                <% tstatust %> : <b><% showStatus s rangev %></b>
                <br/><br/>
                <%showContent lang  s%>
                <%vsep%>
                <b><% tdatesvot %></b>
                <table> 
                    <tr><td><% tinitialvot %></td><td><b><% startT %></b></td></tr>
                    <tr><td><% tfinalvot %></td><td><b><% endT %></b></td></tr>
                </table>
                <%vsep%>
                <% tquestionsasked %>
                <p align="center"><h2><% question lang s %> </h2></p>
        
        </p>
        
        where 
            (endT,startT)= subjectTimeParms lang s
            showStatus s rangev
                | sstatus s==Draft = tdraftexplain
                | rangev==0 = tundervotation
                | rangev > daysBefore s = tacceptingamends ++ ". " ++ show(rangev - daysBefore s)++ tdaystovote
                | rangev > 0 = tundervotation++" "++show rangev++" "++tdaystoend
                | otherwise= tclosed++show (-rangev)++tdaysago

            (tstatust, tquestionsasked, tvvodel, tinitialvot, tfinalvot,tdraftexplain,tclosed,tdatesvot,tundervotation, tacceptingamends, tdaystovote, tdaysago, tdaystoend)=
                    case lang of
                            "es" -> (LangEs.statust, LangEs.questionsasked, LangEs.vvodel, LangEs.initialvot, LangEs.finalvot,LangEs.draftexplain,LangEs.closed,LangEs.datesvot,LangEs.undervotation, LangEs.acceptingamends, LangEs.daystovote, LangEs.daysago, LangEs.daystoend)
                            _    -> (Lang.statust, Lang.questionsasked, Lang.vvodel, Lang.initialvot, Lang.finalvot,Lang.draftexplain,Lang.closed,Lang.datesvot,Lang.undervotation, Lang.acceptingamends, Lang.daystovote, Lang.daysago, Lang.daystoend)
                        



showSubjectHeader lang s email =
     <span>
                <% tauthort %> : <b><% [ <% auth++";" %>   | auth <- authors s] %> </b>

                <br/>
                <% ttopicst %> : <b><% listByComma (Data.topics s) %> </b>
 
                <br/>
                <% if canEdit email s rangev then <% editLink s %> else <span/> %>
    </span>

        where   rangev=unsafePerformIO $ subjectRangeVote s 
                editLink s= <a href= (cgiURL ++ "?op=sub&subject="++sname s++"&type=modify")>
                                                You are the author, edit this proposal
                            </a>
                (tauthort,ttopicst)= case lang of
                        "es"-> (LangEs.authort, LangEs.topicst)
                        _   -> (Lang.authort, Lang.topicst)


{-
showAmedments lang project s email=do
        
        content<- contentIO amends 
        return $ p $ header ++ content

  where
        subject = sname s
        header =  <p align="center"><h1><% tsuggestedmod %></h1> </p>
        contentIO amends= mapM showAmend   amends 
        
        showAmend   name=do
             (Ra  a)<- justGetVResource $! Ra  uSubject{aname=name,sname=subject,prosname=project}

             rangev <- subjectRangeVote  a

             return $ showSubject lang  a  rangev email 
                
        tsuggestedmod= case lang of
                        "es" -> LangEs.suggestedmod
                        _    -> Lang.suggestedmod
-}




showDiffAmedments amends lang project s email=do
        if null amends then return $ <nothing/> 
         else do
          content<- contentIO amends 
          return $ <p> <% header %> <% content %> </p>

  where
        ssubject= sname s
        header =  <p align="center"><h1><% tsuggestedmod %></h1></p>
        contentIO amends= mapM showDiffAmend  amends 

        showDiffAmend name = do
             rangev <-  subjectRangeVote s
             Ra  a  <-  justGetVResource $ Ra uSubject{aname=name,prosname= project,sname= ssubject}
             Rs s   <-  justGetVResource $ Rs uSubject{sname= ssubject,prosname=project} 
             hasVoted<- hasVotedUser email (votes a) project 

             return  $
                      <p>
                          <center> 
                            <a name="amendments"/>
                                        <b><% tamendtoproposal %></b>   
                                        <br/>
                                        <font color="#000080" size="6"><% aname a %></font>
                                  </center>
                                  <a name=(aname a) />
                                  <% showSubjectHeader lang  a email %> 
                                  <h3> Content:</h3> <% contents s a %>
                                  <h3> Topics: </h3> <% change2HTML (topics s  )                (diff1 ( topics   s )( topics a)) %>
                                  <h3> Question:</h3> <%        change2HTML (strToList $ question lang s)       (diff  ( question lang s )( question lang a)) %>
                                  <h3> Options: </h3> <% change2HTML (stringOptions lang s )    (diff1 (stringOptions lang s )(stringOptions lang  a))%>
                                
                                  <% if voteCond s rangev 
                                       then
                                                <p>
                                                        <a name=(aname a ++"vote")><% voteForm1 lang "voteam" name ssubject hasVoted  (amendOptions lang) %></a>
                                                </p>
                                           else 
                                            <span/>
                                  %>
        
                                  <% votation lang (amendOptions lang) (votes a) (sumVotes a ) %>
                                  <% showVoted lang hasVoted (amendOptions lang) %>
                </p>
        
        showop op= show op
        
        contents s sa= case (content s) of
                        Str str         ->change2HTML (lines1 str) (contDiff sa)
                        ConfText str    ->change2HTML (lines1 str) (contDiff sa)
                        --SForm form      ->showDiffForms lang form (contForm sa) 
        contDiff a = xs  where Changes xs = content a
        --contForm a = form where SForm form= content a
        
        (tamendtoproposal,tsuggestedmod)= case lang of
                    "es" -> (LangEs.amendtoproposal,LangEs.suggestedmod)
                    _    -> (Lang.amendtoproposal,Lang.suggestedmod)


        
showSubjectPage env= do
     Just(Rs s) <-  getVResource $ Rs uSubject{ prosname=project,sname= subject} 
     rangev <- subjectRangeVote s
     moresub <- moreSubjects email
     let   tabs = createTabs [freeChooserName] subject "Other proposals:" moresub
     return Content{ 
        mime= page (sname s)  $ pageBody tabs lang $ body s  rangev ,  cookies=[]}

     where

        project = env->>"project"
        subject = env->>"subject"
        email   = env->>"email"


        lang= env->>acceptLang
        body s  rangev  = 
             <p>
                <% showSubject lang s  rangev email %>
                <form action=cgiURL method="post">
                        <input type="submit" name=tsendforvote value=""/>
                        <input type="submit" name=tbacktoedit value=""/>
                        <% hiddenCodify env %>
                </form>
             </p>

              
        (tsendforvote,tbacktoedit)=case env->>acceptLang of
           "es" -> (LangEs.sendforvote,LangEs.backtoedit)
           _    -> (Lang.sendforvote,Lang.backtoedit)




showVoted lang hasVoted options = 
     case( hasVoted) of
        Nothing         ->  <b><% tnotvoted %></b>
        Just (Priority pri,IndexVote 0)    ->  <b><% tnotvoted %></b>
        Just (Priority pri,IndexVote v)    ->  case pri of
                        4   ->     <p><b><% tyouvoted %></b><% votestr v %></p>
                        3   ->     <p><b><% tsubjectvoted %></b><%votestr v %></p>
                        2   ->     <p><b><% ttopicvoted %></b> <% votestr v %></p>
                        1   ->     <p><b><% tprojectvoted %></b><% votestr v %></p>
    where
            votestr v= <h3><% options !!(v-1) %></h3>
            (tsubjectvoted, tyouvoted,tnotvoted,tprojectvoted,ttopicvoted) = case lang of
                    "es" -> (LangEs.subjectvoted, LangEs.youvoted, LangEs.notvoted,LangEs.projectvoted, LangEs.topicvoted)
                    _    -> (Lang.subjectvoted, Lang.youvoted, Lang.notvoted,Lang.projectvoted, Lang.topicvoted)

votation:: String -> [String] -> DiffArray Int PriorIVote -> DiffUArray Int Int -> HSP XML
votation lang options votes sumVotes= 
        <center> 
                <table tableStyle>
                        <caption><% tvotationt %> </caption>
                        <tr><th><%toption%></th><th><% tResult %></th><th><% tpercent %></th></tr>
                        <%[<tr><td><% optioni %></td><td><b><% printf  "%d\n" vi :: String %></b></td>
                            <td><% (printf  "%5.2f\n" (percent1  vi)) ++"%" %></td>
                           </tr> | (vi,optioni)<-zip v (thasnotvoted:options)
                          ]
                        %>
                </table>
        </center>

        where 
                v= elems $ sumVotes 
                percent1 :: Int -> Float
                percent1 x=  (fromIntegral x*100)/fromIntegral (upper+1)  where (0,upper)= bounds votes
                (thasnotvoted, tvotationt, toption, tpercent, tResult)= case lang of
                            "es" -> (LangEs.hasnotvoted,LangEs.votationt, LangEs.option,LangEs.percent, LangEs.result)
                            _    -> (Lang.hasnotvoted,Lang.votationt, Lang.option,Lang.percent,Lang.result)



voteMessage lang s rangev hasVoted 
        |rangev==0 = <b><% tyoucannow %></b>
        |rangev > daysb = <p><h3><% show (rangev-daysb )++tdaystovote %></h3></p>
        |rangev > 0     = <p><h3><% voteNow hasVoted s++show rangev++tdaystoend %></h3></p>
        |otherwise= <h3><% tclosed++show (-rangev)++tdaysago %></h3>

   where        daysb= daysBefore s

                voteNow hasVoted s =
                     case( hasVoted) of
                        Nothing ->  tyoucannow
                        Just v  ->  tyoucanchange
                (tyoucanchange, tdaystoend,tdaysago,tyoucannow,tdaystovote,tclosed)= case lang of
                                "es" -> (LangEs.youcanchange, LangEs.daystoend,LangEs.daysago, LangEs.youcannow, LangEs.daystovote, LangEs.closed)
                                _    -> (Lang.youcanchange, Lang.daystoend, Lang.daysago, Lang.youcannow, Lang.daystovote, Lang.closed)


showVoteSubjectPage env cookies msg= do

     [Rs s, Rp pr] <- justGetVResources [Rs uSubject{sname=subject,prosname=project},Rp uProject{pname=project}]
     rangev <- subjectRangeVote s
     moresub <- moreSubjects email
     let tabs = createTabs [freeChooserName,project] subject "Other proposals:" moresub  
     if null email then  return Content{ mime= page subject  $ pageBody tabs lang $ showSubject lang s  rangev email, cookies=[]}
        else do
                hasVoted        <- hasVotedUser email (votes s) project
                delegatedTable  <- delegates lang project email s
                amends          <- getAmends project $ sname s
                amedments       <- showDiffAmedments amends lang project s email
                return Content{mime= page subject  $ pageBodyPr tabs  (showTextContent $ pdescrip pr)  (rigthSide lang)  
                                        $ body s rangev hasVoted delegatedTable amends amedments,  cookies=cookies}

     where

        email   = env ->>"email"
        subject = env->>"subject"
        project = env->>"project"

        category= env ->> "category"
        lang= env->> acceptLang
        
        body:: Subject->Int-> Maybe PriorIVote -> HSP XML -> [String] ->  HSP XML -> HSP XML
        body s  rangev hasVoted delegatedTable amends amedments= 
            <p><%anchors lang amends %>
                <font color="FF0000" size="4"><b><% msg %></b></font>
                <% if sstatus s == Draft 
                    then 
                        <% showSubject lang s  rangev email %> 
                    else 
                           <p>
                                <% showSubject lang s  rangev email %>
                                <%vsep%>
                                <% voteMessage lang s rangev hasVoted %>
                                <% if voteCond s rangev 
                                    then
                                        <p>
                                          <a href= (cgiURL++"?op=sub&subject="++subject++"&type=amend")><b><% tsuggestmodif %></b></a> 
                                          <% voteForm1 lang "vote" "" subject hasVoted strOpts %>   
                                        </p>
                                        else 
                                          <nothing/>
                                %>
                                <% votation lang strOpts (votes s) (sumVotes s) %>
                                <% showVoted lang hasVoted strOpts %>   
                                <%vsep%>
                                <h1><% tdelegate %></h1>
                                <p align="justify"><% texplaindel %> </p>
                                <% delegatedTable %>
                                <% amedments %>
                          </p>
                        %>
            </p>        
           where        strOpts=  stringOptions lang s


        delegates lang project email s=do
                Ru us <- justGetVResource $! Ru uUser{Data.name=email}
                return $
                  <form action=(cgiURL++"?op=vsub") method="post">
                   <p align="center">
                        <a name="delegate"/>
                        <input type="hidden" name="op" value="delegate"/>
                        <table tableStyle> 
                            <caption><% tdelegatest %></caption>
                            <tr><th><% tdeltype %></th><th><% tdelemailt %></th><th><% tchange %></th><th/></tr> 
                           
                            <tr><td><b><% tonlyfor++ tstrproject %></b></td>
                                    <td> <% objectUserDel lang us project uProjects %></td>
                                    <td><input type="hidden" name="type" value="project" />
                                        <input type="text" onfocus="this.value=''" name=project value=tnewemail />
                                   </td>
                                   <td> <input type="submit" name=tchangebut value=tchangebut /> </td>
                            </tr>
                            <tr><td><b> <% tonlyfor ++ tstrsubject %> </b> </td>
                                    <td><% objectUserDel lang us (sname s) usubjects %></td>
                                    <td><input type="hidden" name="type" value="subject"/>
                                        <input type="text" onfocus="this.value=''" name=(sname s) value=tnewemail/>
                                    </td>
                                    <td><input type="submit"  name="change" value=tchangebut/></td>
                            </tr>
                            <% map (perTopic us)(Data.topics s) %>
                        </table>
                   </p>
                  </form>
                  where perTopic us topic=
                                   <tr> 
                                                <td> <b><% "Delegated for \""++topic++"\"" %> </b></td>
                                                <td>
                                                     <% case(topicDelegatedTo us topic)of
                                                                   Nothing  -> tnewemail
                                                                   Just user-> user
                                                             %>
                                                </td>
                                                <td><input type="hidden" name="type" value="topic" />
                                                     <input type="text" onfocus="this.value=''" name=topic value=tnewdeluser />
                                               </td>
                                               <td><input type="submit" name="change" value=tchangebut /></td>
                                       </tr>



        (tsuggestmodif, tstrsubject, tstrproject, tonlyfor,tnone, tnewdeluser,tnewemail,texplaindel, tdeltype, tdelemailt,tdelegate, tdelegatest,tchangebut,tchange)= case env->>acceptLang of
                "es" -> (LangEs.suggestmodif, LangEs.strsubject, LangEs.strproject, LangEs.onlyfor, LangEs.none, LangEs.newdeluser, LangEs.newemail, LangEs.explaindel, LangEs.deltype, LangEs.delemailt,LangEs.delegate, LangEs.delegatest,LangEs.changebut, LangEs.change)
                _    -> (Lang.suggestmodif, Lang.strsubject, Lang.strproject, Lang.onlyfor, Lang.none, Lang.newdeluser, Lang.newemail, Lang.explaindel, Lang.deltype, Lang.delemailt,Lang.delegate,Lang.delegatest,Lang.changebut, Lang.change)




registrationPage env msg= Content{ mime=regValHTML env msg, cookies =[] }

validationPage env msg= Content{ mime=regValHTML env msg, cookies =[] }


regValHTML env msg= do

    page strregval  $ pageBody tabs lang $                                                                                                                                                                                                                                                                                           
      <center>
        <h4><% tyoumustlogin %></h4> 

        <font color="FF0000" size="2">
                <% case msg of
                        [] -> <b><% tenteruser %></b>
                        _  -> <b><% msg %></b>
                %>
        </font>

        <form action=cgiURL method="post">  
                <table> 

                    <tr><td><input type="hidden" name="op" value="vor" /></td></tr>
                    <tr><td><b><% temailt %></b></td><td><input type="text" name="email" value=""/></td><td></td></tr>
                    <tr><td><b><% tpasswordt %></b></td>
                        <td><input type="password"  name="pass"/></td>
                        <td></td>
                        <td><input type="submit" name="val" value=tvalidatet/></td>
                    </tr>

                    <% blankRow %>
                    <tr><td colspan="3" ><h3><% tenteragain %></h3></td></tr>
                    <tr><td><b><% tpasswordt %></b></td><td><input type="password" name="pass2"/></td><td><b><% tagainto %></b></td><td><input type="submit" name="reg" value=tregister /></td></tr>
                        
                </table>
                <% hiddenCodify env %>
        

        
      </form>

     </center>
     where
        tabs = createTabs [freeChooserName] strregval "" []
        lang= env->>acceptLang
        (strregval,tyoumustlogin, tvalidatet, tregister, tpasswordt,tagainto,temailt,tenteragain,tenteruser)= 
            case lang of
                    "es" -> (LangEs.strregval,LangEs.youmustlogin, LangEs.validatet, LangEs.register, LangEs.passwordt, LangEs.againto, LangEs.emailt, LangEs.enteragain,LangEs.enteruser)
                    _    -> (Lang.strregval,Lang.youmustlogin, Lang.validatet, Lang.register, Lang.passwordt, Lang.againto, Lang.emailt, Lang.enteragain, Lang.enteruser)


                          
                                  
