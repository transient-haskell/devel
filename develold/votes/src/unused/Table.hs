module Table where
import HSP
import HSPClientside
import Data.List(sortBy)
import System.IO.Unsafe
import HTTPParser
import Data
import Vote
import Lang
import LangEs
import LangEs(acceptLang)
import UtilHTML
 

----------------test only-----
import CGI


cgi= CGI test

test1 _ = return $ show $   <h1> <% unsafeRunHSP  <b> hola </b> %> </h1>

test env= do 
    return $ show  $ showTable ["head1","head2","head3"] (2, GT)
           [[itext "val1" , <b>val2</b>, itext <%"val3"%>]
           ,[itext "val4" , <b>val5</b>, itext "val6"]]

--------------------------

itext :: String -> HSP XML
itext t= <t> <% t %> </t>



listPublicProjects =do
    pprs <- publicProjects
    return $ listProjects pprs


userPage env cookies msg= do
  print "en userPage"
  list <- case (lookup "email" env, lookup "project" env) of
           (Nothing, Nothing) -> listPublicProjects
  
           (user,Nothing) -> listProjectsUser env
           (user,project) -> listSubjectsProject env
          
  let html = <p> <font color="550000" > <% msg %> </font> 
                <b>List of projects you have access to: </b>
                <%list%>
                <a href=(cgiURL++"?op=pro&type=create")>create your own project</a>
            </p>
  return $ Content html  cookies 

philosophyPage = Content <p> philosophy </p> []

modifySubjectPage:: Env -> Cookies -> String ->  IO (CgiOut (HSP XML))
modifySubjectPage env cookies  msg =
   if null email then return $ validationPage (("backto","modifySubject"):env) "" else do 
          Rp pr <- justGetVResource (Rp uProject{pname= project})
          Just (Rs s) <- case types  of
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

modifySubjectPage1 :: Env -> Cookies -> Subject -> String -> IO (CgiOut (HSP XML))
modifySubjectPage1 env cookies sub  msg= do
       Rp pr <- justGetVResource $ Rp uProject{pname=prosname sub}
       let cats= categories pr
       return $ Content (html  cats) cookies
       where
       
        html cats= 
             <p>
                <input type="hidden" name="op" value="suc"/> 
                <input type="hidden" name="type" value=types/>
                <input type="hidden" name="backTo" value=(env->>"backTo")/>
                <font color="FF0000" size="4"><% msg %></font>
                <h1><% case types of{"modify"-> Lang.editingsub; "create"->Lang.propcreation;"amend"->"Amending: "} ++ (sname sub) %></h1>
                <%showTable header (0,GT)  $ resultSet  cats %>
            </p>
       
        header=["Name","value"]

        types= env->>"type"
        lang= env->>acceptLang
        -- (endT,start









T)= subjectTimeParms lang sub
        resultSet  cats=  
               [[itext "name"],[ <%nameContent%>]           
               -- ,[itext "project"],[input "project" $ prosname sub]
               ,[itext "category"],[  categoryContent cats] 
               ,[itext "authors"],[input "authors" $ show $ authors sub]
               ,[itext "topics"],[input "topics" $ show $ topics  sub] 
               ,[itext "status"],[input "status" $ show $ sstatus sub]
               ]
        nameContent=case  env->>"type" of
         "create" -> -- edit name, category
            <input type="itextfield" size="80" name="name" value=""/>
         "amend"  ->
            <input type="itext" size="80" name="aname"  value=""/>
         "modify" ->
            <input type="hidden" name="name" value=(sname sub)/> 
                           
                                   
        categoryinput cats= 
           <p><%[<p><input type="radio" checked=(if x == category sub then True else False) 
                            name="category" value=(show x)/> <b><% x %></b>
                      </p> | x <- cats]%>
           </p>  -- get categories dynamically
                                      
        categoryContent cats=case types of
         "create" -> categoryinput cats
       
         "amend" -> <p>
                           <%category sub%>
                           <input type="hidden" name="name" value=(sname sub)/>
                           <input type="hidden" name="category" value=(category sub) />
                   </p>
          
         "modify" ->
                  if category sub == "Constitutional" then <input type= "hidden" name= "category" value= "Constitutional" />
                      else categoryinput cats
                                  
   
   
        input n v=  <input type="itext" value=v name=n/>
      

--listProjectsUser:: [(String->String)] -> HSP XML
listProjectsUser env= do
   resultSet <- projectList $ env ->>"user"
   return $ listProjects resultSet
   
listProjects resultSet=  showTableResults header (0,GT) (map getFields resultSet) 0 10
   where
   header=["Name","Topics","N. Proposals", "Under votation"]
   
   getFields pr= [itext $ pname pr, itext $ show $ ptopics pr, itext $ show . length $ psubjects pr, itext $ active pr ]
   active pr= "TBD" 
   
listSubjectsUser env= do
   resultSet <- subjectList user
   return $ showTableResults header (0,GT) (map getFields resultSet) 0 10

   where
   header=["Name","Topics","N. amends", "status"]
   user=env->>"email"
   project= env->>"project"
   subject= env->>"subject"
   getFields sub= [itext $ sname sub, itext . show $ topics sub
                  , itext . show . length . unsafePerformIO $ getAmends project subject
                  , itext .show $ sstatus sub ]
   
   
listSubjectsProject env= listSubjectsProjectName (env ->>"project") where
 listSubjectsProjectName project= do
   Rp pr <- justGetVResource $ Rp uProject{pname=project}
   
   resultSet <- justGetVResources[Rs uSubject{sname= sub, prosname=project} | sub <- psubjects pr]
   return $ showTableResults header (0,GT) (map getFields resultSet) 0 10
   
   where
   header=["Name","Topics","N. amends", "status"]
   subject= env->>"subject"
   project= env->>"project"
   getFields (Rs sub)= 
                  [itext $ sname sub, itext . show $ topics sub 
                  ,itext . show . length . unsafePerformIO $ getAmends project subject
                  ,itext . show $ sstatus sub]

listDelegates env= do
   Ru us <- justGetVResource $ Ru uUser{Data.name=user}
   let resultSet= (map forProject $ uProjects us) ++ (map forSubject $ usubjects us) ++ (map forTopic $ utopics us)
   return $ showTableResults header (0,GT)  resultSet 0 10

   where
   user= env->>"email"
   header=["type","name","delegate"]
   
   forProject= getFields "Project"
   forSubject= getFields "Subject"
   forTopic=   getFields "Topic"
   getFields t op= [itext t,itext $ uObject op, 
                           <form action=(cgiURL++"?op=delegate")>
                                        <input type="itext" name="type" value=(delegatedTo op) onChange="submit()"/>
                           </form>]

showVoteSubjectPage env cookies msg= do
   Rs sub <- justGetVResource $ Rs uSubject{sname=subject,prosname=project}
   rangev <-  subjectRangeVote sub
   let (endT,startT)= subjectTimeParms lang sub
   let resultSet=  
               [[itext "name",itext $ sname sub]           
               ,[itext "project",itext $ prosname sub]
               ,[itext "category",itext $ category sub] 
               ,[itext "authors",itext $ show $ authors sub]
               ,[itext "topics",itext $ show $ topics  sub] 
               ,[itext "status",itext $ show $ sstatus sub]
               ,[itext "tinitialvot",itext $ show startT]
               ,[itext "tfinalvot",itext $ show endT]
               ]

   let html= <p> <% showTable header (0,GT)  resultSet %>
                 <% showContent lang sub %><br/><br/>
                 <% stringOptions lang sub %><br/><br/>
                 <% question lang sub%><br/><br/>
   	         <% voteForm lang user  sub rangev project%>
            </p>
   
   return $ Content html cookies
   
   where
   lang= env->>acceptLang
   user= env->>"email"
   project= env->>"project"
   subject= env->>"subject"
   header=["name","value"]
   
             
          

                                 
          

showProject env=do
  Rp pr <- justGetVResource $ Rp uProject{pname=project}
  let resultSet=
            [[itext "name",itext $ pname pr]     
            ,[itext "author",itext $ pauthor pr]  
            
            ,[itext "topics",itext .  show $ ptopics pr]
            --,[users pr]
            --psubjects 
            ,[itext "Public?",itext . show $  public pr]
            ,[itext "Visible?",itext . show $ visible pr]
            ]

  return <p> showTableResults header (0,GT)  resultSet 0 10
             showHtmlContent $ pdescrip pr
             
             listSubjectsProjectName project
         </p>
  
  where
  project= env->>"project"
  header=["name","value"]
              

modifyProjectPage env cookies msg= do
   print email
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


modifyProjectPage1 :: Env -> Cookies -> Project -> String -> IO (CgiOut (HSP XML))
modifyProjectPage1 env cookies pr msg=do 
 print "en modifyProjectPage" 
 return $ Content ( html  pr msg) cookies
 where

  html pr msg=
    <p><font color="FF0000" size="4"> <b><% msg %></b> </font>
       <form action=(cgiURL++"?op=cre") method="post"> 
        <input type="hidden" name="op" value="cre"/>
        <input type="hidden" name="oldname" value=( if types=="modify" then  (pname pr) else "")/>
        <% showTable header (0,GT)  
                [[itext "name",    <input type="itext" name="name" value=(pname pr)/>]     
                ,[itext "ptopics", <input type="itext" name="topics" value= (show $ ptopics pr) />]
                ,[itext "users",   <input type="itext" name="topics" value= (show $ users pr) />]
                ,[itext "Public",  <input type="checkbox" name="ispublic" value=(show $ public pr)/>]
                ,[itext "Visible", <input type="checkbox" name="ispublic" value=(show $ visible pr)/>]
                ] %>
        
	<b>Descripción</b><br/>
	<textarea name="pdescrip">pdescrip pr</textarea>

        <input type="hidden" name=s"subjects" value= (show $ psubjects pr) />
        
        <input type="hidden" name="email" value= (env->>"email")/>    
        <input type="hidden" name="type" value= (env->>"type")/>
        <input type="hidden" name="op" value="cre"/>
        <%if env->>"type"=="modify" then <input type="hidden" name="oldname" value=( env->>"project")/>
           else <b/>
        %>
        <input type="submit" name="OK" value="OK"/><input type="reset" name="cancel" value="cancel"/>

      </form>
   </p>
   
  
  types= env->>"type"
  header=["name","value"]

--showTable:: [String] ->(Int,Ordering)-> [[HSP XML]]->HSP XML
showTable header (indexord,upDown) elems=

  <table class="nv-datagrid">
        <thead>
          <tr>
	  <%[
              <th>
        	  <p align="center"><b><% x %><%updown i%></b></p>
              </th>
	      | (x,i)<-zip header [1,2..]
	  ]%>
	  </tr>
	</thead>
	<tbody>
	<%[
	   <tr>
		<%[<td> <% elem %> </td> | elem<-row]%> 
	   </tr>  | row<-elems
	]%>
	</tbody>
  </table>

  where

	updown i=if i== indexord 
			 then case upDown of
					GT->"&#8593;"
					LT->"&#8595;"
			 else " "


sort set index upDown=
  case upDown of
  	GT-> sortBy compareRowGT set
  	LT-> sortBy compareRowLT set
  where
        compareRowGT row1 row2 = compare (row1!!index) (row2!!index) 
	compareRowLT row1 row2 = compare (row2!!index) (row1!!index) 
										
showTableResults header (index,upDown) resultSet from n = 
  showTable  header (index,upDown) $ chunk resultSet from n
  
 where
  chunk set from n= 
     let (_,rs)=splitAt from set
   	 in   take n rs

{-
como gestionar las listas de resultados? con un flow?
no es necesario? showTableResults y lazy evaluation lo gestionan? supongo que si. comprobarlo.
-}
registrationPage env msg= Content (regVal env msg) []

validationPage env msg= Content (regVal env msg) []

regVal env msg= 
      <p align="center">
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
                    <tr><td><b><% temailt %></b></td><td><input type="itext" name="email" value=""/></td><td></td></tr>
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

     </p>
     where
        lang= env->>acceptLang
        (tyoumustlogin, tvalidatet, tregister, tpasswordt,tagainto,temailt,tenteragain,tenteruser)= 
            case lang of
                    "es" -> (LangEs.youmustlogin, LangEs.validatet, LangEs.register, LangEs.passwordt, LangEs.againto, LangEs.emailt, LangEs.enteragain,LangEs.enteruser)
                    _    -> (Lang.youmustlogin, Lang.validatet, Lang.register, Lang.passwordt, Lang.againto, Lang.emailt, Lang.enteragain, Lang.enteruser)
