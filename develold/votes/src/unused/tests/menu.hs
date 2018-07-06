{-# OPTIONS -F -pgmFtrhsx -fglasgow-exts #-}
module Main where
import HSP
main= do
  htm <- unsafeRunHSP $ tabs ["FreeChooser","grupo1"] "proposal" ["proposal2","proposal3","proposal4"]
  print htm
 
tabs ants op options=

       <div id="divTabs">
      	        <ul>
      		  <% [<li class="" > <span class="tabName" ><% i %></span><span class="arrow"> > </span></li>| i <- ants] %>			  
      		      <li class="selected"  ><span class="tabName" ><% op %></span></li>
      		      <li class="blank"/>
      		      <li class="blank"> Other  <% level %> </li>
      		  <% [<li class="" ><span class="tabName" ><% i %></span></li> | i <- options] %>
      		</ul>
      	
       </div>
       where
       level = case length ants of
          2 -> "proposals"
          1 -> "groups"	

putTabs env=
    case (project, subject, email) of
       ("","","") -> tabs [] freeChooser ["login/register link"]
       (project,"",email) -> tabs [freeChooser] project moreProjects
       (project,subject,email -> tabs [freeChooser, project] subject moreSubjects
    where
    project= env ->> project
    subject= env ->> subject
    email  = env ->> email 