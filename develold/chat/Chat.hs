{-# OPTIONS -F -pgmFtrhsx -fglasgow-exts #-}
module Chat (page) where
--import CGI
--import TCache
import HJScript hiding (test)
import HJScript.DOM
import HSP hiding (appChild)
import HSP.HJScript
import CGI
import HTTPParser

page = CGI page2

page2 :: [(String,String)] -> IO String
page2 _=  do
       let str= show $  Content{mime= page1 ,  cookies=[]}
       putStrLn str
       return str
       
page1 :: HSP XML
page1=
        <html>
        <frameset rows = "75%,*">
          <frame src ="http://localhost/chat1.hs" />
          <frame src ="mercur.htm" /> 
        </frameset>
        </html>
  
  
  
{-
type Who Time Text= String
data ChatContent= ChatConent Who Time Text deriving (Read, Show)
instance IResource ChatContent where
   KeyResource _= "ChatContent"
   
chat = CGI chatSched


chatSched :: [(String,String)] -> IO String
chatSched env=
    withResource (ChatContent undefined) doit
    



makeP x = <p><b><% x %></b></p>

page :: HSP XML
page =
      <html>
        <head><title>Add paragraph</title></head>
        <% do
                  (r,b) <- ref <body><%
                                 makeP "Paragraph created Server-side!"
                                    %></body>
                  let fun = do
                           let x = document # getElementById (string r)
                           x <:  (makeP  "Paragraph created Client-side!")
                           return ()
                  b <: (<button>Click Me!</button> `onClick` fun)
         %>
      </html>

-}
  
  
