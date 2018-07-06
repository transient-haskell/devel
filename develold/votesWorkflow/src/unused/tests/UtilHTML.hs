{-# OPTIONS -F -pgmFtrhsx -fglasgow-exts #-}
module UtilHTML where
import Data.List
import HSP
import Data
import Vote
import Lang
import LangEs
import HTTPParser ((->>))

showChanges l= 
              <p>
                 <% [ 
                        <p>
                          <b>Deleted:</b>
                          <br/>
                          <% x %>
                          <br/>
                          <b>Added</b>
                          <br/>
                          <% y %>
                          <br/>
                        </p> 
                        |  Change _ xs ys <- l, x<-xs, y<-ys]
                  %>
               </p>
    
change2HTML xs diff= <p><% concatMap showHTML1 $ diff2Changes xs diff 0  %> </p> where

	showHTML1 (Normal x)	= map (\t -> <p><% t %></p>) x 
	showHTML1 (Deleted x)	= map (\t-> <p><strike> <%t %></strike></p>) x 
	showHTML1 (Added x) 	= map (\t-> <font color= "FF0000" size="3"><% t %><br/></font>) x
	

blankRow :: HSP XML
blankRow= <tr><td><br/></td></tr>
vsep :: HSP XML
vsep=	<p><br/><center>-.-</center><br/></p>



tableStyle = [	"border" := "3", "cellspacing" := "0", 
		"width" := "100%", "id" := "AutoNumber1", 
		"style" := "border-collapse: collapse", 
		"bordercolor" := "#111111", "cellpadding" := "2"]
		

pageTableStyle=  ["width" := "100%", "cellspacing" := "9" ]	

listByComma= foldr (\a b->if null b then a else a++", "++b) ""


 
showTextContent str= replace str "\n" "<br/>"


showContent lang s= 
        case (content s) of
                        Str string      -> <span> <% [ <p><% l %></p> | l <- lines string] %> </span> -- <span><% replace string "\n" "<br/>" %> </span>
                        ConfText string   -> <span> <% [ <p><% l %></p> | l <- lines string] %> </span>  -- <span><% replace string "\n" "<br/>" %> </span>
                        Changes changes -> showChanges changes
                        --SForm form      -> wDisplay lang form

-- True if the subject is in votation time
voteCond s rangev   |rangev>=0 = True
                    |otherwise = False

voteForm lang email s rangev project= do
        hasVoted<- hasVotedUser email (votes s) project
        return $ 
         <% if voteCond s rangev then
            <p>
                
                <% voteForm1 lang "vote"  "" (sname s) hasVoted  (stringOptions lang s) %>
            </p>
           else <p/>
         %>  
         
voteForm1:: String -> String -> String-> String-> Maybe PriorIVote -> [String] -> HSP XML
voteForm1 lang op aname subject hasVoted options =
        <p>
            <b><% tvotethis %></b>
            <a name="vote"/>
            <br/>

            <center>
                <h4><% toptionst %></h4>
      
                <form action=cgiURL method="post">
                        <input type="hidden" name="op" value=op /><input type="hidden" name="subject" value=subject/>
                        <%if op=="voteam" then <input type="hidden" name="aname" value=aname/> else  <span/> %>
                        <table >
                                  <%[<tr><td><input type="radio" name="option" value=value/></td>
                                         <td><% formatOp value %></td>
                                             <td><a href=  (cgiURL++"?op=mess&opt="++value++"&subject="++subject)> 
                                                     <% tsupportmess %>
                                                 </a>
                                             </td>
                                     </tr>
                                     |  value <- options]
                                  %>
                                  
                                  <% if op== "vote" 
                                      then 
                                                    <tr><td><% tyoucanalso %></td>              
                                                             <td></td>
                                                             <td><a href= (cgiURL++"?op=sub&subject="++subject++"&type=amend")>
                                                                                              <b><% tsuggestmodif %></b>
                                                                     </a>
                                                            </td>
                                                   </tr>
                                      else <tr></tr>
                                  %>
                        </table>
                        <br/>
                        
                        <br/>
                        <input type="submit" name="Vote" value=tvote/>
                </form>
           </center>
        </p>
   where

        formatOp  op = case (isPrefixOf "http" $ dropWhile (==' ') s2)of
                                True    -> <a href=s2> <b><% s1 %></b></a>
                                False   -> <span><b><% s1 %></b> - <% s2 %></span>

                        where (s1,(c:s2))=case(findIndex (==',') op) of
                                        Just i  -> splitAt i op
                                        Nothing -> (op,",")
        (toptionst,tvotethis,tvote, tsuggestmodif,tyoucanalso,tsupportmess)= case lang of
                    "es" -> (LangEs.optionst, LangEs.votethis, LangEs.vote, LangEs.suggestmodif,LangEs.youcanalso,LangEs.supportmess)
                    _    -> (Lang.optionst, Lang.votethis, Lang.vote,Lang.suggestmodif, Lang.youcanalso, Lang.supportmess)



amendOptions lang=case lang of 
                "es"->[LangEs.yes,LangEs.no, LangEs.complaint]
                _   ->[Lang.yes,Lang.no, Lang.complaint]
                

hiddenCodify env = <p><%[ <input type="hidden" name=name value=value/> | (name,value) <-  env1]%></p> where
                       (env1, _) = Vote.myreads1 env (\(x,y)-> x/= "HTTP_COOKIE")   
                       
                   
{-
putTabs level env =
    case (level, project, subject, email) of
       (LTop, _, _, _) -> tabs [] freeChooserName ["login/register link"]
       (LProject,"",_,_) -> publicProjects >>= \prs -> return $ tabs [] freeChooserName ["login/register link"]
       (LProject,project,_,"") -> publicProjects >>= \prs -> return $ tabs [freeChooserName] project prs
       (LProject,project,_,email) -> moreProjects >>= \prs -> return $ tabs [freeChooserName] project prs

       (LSubject,project,subject,email) ->  moreSubjects >>= \subs -> return $ tabs [freeChooserName, project] subject subs
    where
    project= env ->> project
    subject= env ->> subject
    email  = env ->> email -} 
moreProjects email= case email of
   "" -> do pub<- publicProjectNames
            return $ take 3 pub
   email -> do
       prs <- projectListNames email
       let l= length prs
       if l < 3 then do pub <- publicProjectNames
                        return $ prs ++ take (3 - l) pub
                else return $ take 3 prs

moreSubjects email= do
    prs <- projectListNames email
    return $ take 3 prs
 

 
createTabs :: [String]-> String -> String -> [String] -> HSP XML           
createTabs ants op msg options=

       <div id="divTabs">
      	      <ul>
      	        <% [<li class="" > <a href=(link i)><span class="tabName" > <% opt %> </span><span class="arrow"> > </span></a> </li> 
      	                 | (opt,i) <- zip ants [1..]] %>			  
      		<li class="selected"  ><span class="tabName" ><% op %></span></li>
      		<li class="blank"/>
      		<	li class="blank"> <% msg %> </li>
      		<% [<li class="" ><a href=(link1 )><span class="tabName" ><% i %></span></a></li> | i <- options] %>
              </ul>
       </div>
       where
       h= ["none","project","subject"] 
       zhant=  zip h ants    
       link1  = linkTo  zhant
       link i = linkTo $ zip h (take i ants++ repeat "")
       linkTo cs= cgiURL++"?"++ concatMap linkTo1 cs where
       linkTo1 (n,v)= n++"="++v++"&"
