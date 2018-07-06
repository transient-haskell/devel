module Main where                                                                                                                                                                                                                                                                                                                                                                                                                                       

import System.Plugins

main= do
      s <-loadExec "eval.o" "mainc"  
      print s
      
loadExec:: String-> String->IO String          
loadExec file method = do    
    
               mv <- load file ["."] [] method
               case mv of
                LoadSuccess mod v ->    v  :: IO String
                LoadFailure msg   ->    return $ concat msg
                
env :: [(String,String)]
env=       [("op","cre"),("oldname",""),("type","create"),("name","prueba"),("pdescrip","desc")
           ,("topics","t1,t2"),("users","")
           ,("propTypesStr","[(\"OrdinaryProposal\", \r\n   Lambda ( Workf (\\(action, cache) e -> do\r\n     unsafeIOtoWF $ setCache cache\r\n     e' <-   action (Action Propose []) e\r\n     e''<-   action (Action AmendDefault []) e\r\n     e'''<-  action (Action (VoteDefault \r\n              Majorities\r\n               {percentAprobal= 50\r\n       \t       ,percentNecessary = 50\r\n\t       ,percentComplaint = 10\r\n       \t       ,votationTime = 10\r\n\t       ,timeSpan= 30}\r\n\t      True) []) e''\r\n\t      \r\n     return e'''\r\n )))]\r\n\r\n"),("subjects",""),("ispublic","OFF"),("isvisible","OFF"),("OK","OK"),("REQUEST_METHOD","POST"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING","op=cre"),("Host","localhost"),("User-Agent","Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.6) Gecko/20071008 Ubuntu/7.10 (gutsy) Firefox/2.0.0.6"),("Accept","text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),("Accept-Language","en-us,en;q=0.5"),("Accept-Encoding","gzip,deflate"),("Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.7"),("Keep-Alive","300"),("Connection","keep-alive"),("Referer","http://localhost/FreeChooser.cgi?op=pro&type=create"),("Cookie","email=Alberto; project=sample1"),("Content-Type","application/x-www-form-urlencoded"),("Content-Length","817"),("project","sample1"),("email","Alberto")]

