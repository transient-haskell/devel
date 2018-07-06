module Main where
import Control.Exception(finally)
import System.IO (hClose)
import System.Exit
import System.Environment
import Control.Workflow(Stat,startWF)
import WorkflowElem 
import Data
import Vote
import FreeChooser
import Data.TCache.Dynamic(registerType)

import Unsafe.Coerce
import Control.Concurrent(threadDelay)

main=do
   --str <- cgi1 crUser
   
   --str <- cgi1 crGr
   
   --str <- cgi1 crPr
   
   str <- cgi1 amend
   threadDelay  100000000000


--registerType1 :: IO a
--registerType1 = let  x= unsafeCoerce 1 in  registerType x `seq` return x

mainWF=do
   --registerType ::   IO (Rs blankSubject)
   registerType ::   IO (Stat ResourceVote)
   
   execWorkflow $ Rs blankSubject{category="OrdinaryProposal", prosname="Grupo", sname="Proposal3"}
  

crUser=[("op","vor"),("email","Alberto"),("pass",""),("pass2",""),("reg","register"),("op","vor"),("email",""),("pass",""),("val",""),("REQUEST_METHOD","GET"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING","op=vor&email=&pass=&val="),("User-Agent","Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/0.4.154.29 Safari/525.19"),("Referer","http://magovir/FreeChooser.cgi?none=FreeChooser.com&project=&subject=&"),("Accept","text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),("Accept-Encoding","gzip,deflate,bzip2,sdch"),("X-SDCH","Chrome 0.4.154.29"),("Cookie","email=Alberto; project=grupo; subject=proposal1"),("Accept-Language","es-ES,es"),("Accept-Charset","ISO-8859-1,*,utf-8"),("If-Modified-Since","Tue Dec 23 19:35:18 CET 2008"),("Host","magovir"),("Connection","Keep-Alive"),("subject","proposal1"),("project","grupo"),("email","Alberto"),("REQUEST_METHOD","POST"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING",""),("User-Agent","Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/0.4.154.29 Safari/525.19"),("Referer","http://magovir/FreeChooser.cgi?op=vor&email=&pass=&val="),("Cache-Control","max-age=0"),("Content-Type","application/x-www-form-urlencoded"),("Accept","text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),("Accept-Encoding","gzip,deflate,bzip2,sdch"),("X-SDCH","Chrome 0.4.154.29"),("Cookie","email=Alberto; project=grupo; subject=proposal1"),("Accept-Language","es-ES,es"),("Accept-Charset","ISO-8859-1,*,utf-8"),("Host","magovir"),("Content-Length","908"),("Connection","Keep-Alive"),("subject","proposal1"),("project","grupo"),("email","Alberto")]
crGr=[("op","cre"),("oldname",""),("type","create"),("name","grupo"),("pdescrip","desc"),("topics","t1,t2"),("users",""),("propTypesStr","[(\"OrdinaryProposal\", \r\n   Lambda ( Workf (\\action e -> do\r\n     e' <-   action  Propose  e \r\n     action  (VoteDefault \r\n              Majorities\r\n               {percentAprobal= 50\r\n       \t       ,percentNecessary = 50\r\n\t       ,percentComplaint = 10\r\n       \t       ,votationTime = 10\r\n\t       ,timeSpan= 30}\r\n\t      True)  e'\r\n\r\n )))]\r\n\r\n"),("subjects",""),("ispublic","OFF"),("isvisible","OFF"),("OK","OK"),("REQUEST_METHOD","POST"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING","op=cre"),("User-Agent","Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/0.4.154.29 Safari/525.19"),("Referer","http://magovir/FreeChooser.cgi?op=pro&type=create"),("Cache-Control","max-age=0"),("Content-Type","application/x-www-form-urlencoded"),("Accept","text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),("Accept-Encoding","gzip,deflate,bzip2,sdch"),("X-SDCH","Chrome 0.4.154.29"),("Cookie","project=grupo; subject=proposal1; email=Alberto"),("Accept-Language","es-ES,es"),("Accept-Charset","ISO-8859-1,*,utf-8"),("Host","magovir"),("Content-Length","602"),("Connection","Keep-Alive"),("email","Alberto"),("subject","proposal1"),("project","grupo")]
crPr=[("op","suc"),("type","create"),("backTo",""),("project","grupo"),("name","proposal"),("category","OrdinaryProposal"),("authors","Alberto"),("topict1","OFF"),("topict2","OFF"),("contentType","text"),("content","content"),("typeOption","approbal"),("question","Do you agree with this proposal?"),("options","Yes\r\nNo\r"),("noptions","1"),("status","Processing"),("submit","Create/modify Draft/Submit to the group"),("REQUEST_METHOD","POST"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING",""),("User-Agent","Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/0.4.154.29 Safari/525.19"),("Referer","http://magovir/FreeChooser.cgi?op=sub&type=create"),("Cache-Control","max-age=0"),("Content-Type","application/x-www-form-urlencoded"),("Accept","text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),("Accept-Encoding","gzip,deflate,bzip2,sdch"),("X-SDCH","Chrome 0.4.154.29"),("Cookie","subject=proposal1; email=Alberto; project=grupo"),("Accept-Language","es-ES,es"),("Accept-Charset","ISO-8859-1,*,utf-8"),("Host","magovir"),("Content-Length","325"),("Connection","Keep-Alive"),("project","grupo"),("email","Alberto"),("subject","proposal1")]

amend=[("op","suc"),("type","amend"),("backTo",""),("project","grupo"),("aname",""),("name","proposal"),("category","\"OrdinaryProposal\""),("authors","Alberto"),("topict1","OFF"),("topict2","OFF"),("contentType","text"),("content","content \r\nadded"),("typeOption","approbal"),("question","Do you agree with this proposal?"),("options","Yes\r\nNo\r"),("noptions","1"),("submit","Create/modify Draft/Submit to the group"),("REQUEST_METHOD","POST"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING",""),("User-Agent","Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/0.4.154.29 Safari/525.19"),("Referer","http://magovir/FreeChooser.cgi?op=sub&subject=proposal&type=amend"),("Cache-Control","max-age=0"),("Content-Type","application/x-www-form-urlencoded"),("Accept","text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),("Accept-Encoding","gzip,deflate,bzip2,sdch"),("X-SDCH","Chrome 0.4.154.29"),("Cookie","email=Alberto; subject=proposal2; project=grupo"),("Accept-Language","es-ES,es"),("Accept-Charset","ISO-8859-1,*,utf-8"),("Host","magovir"),("Content-Length","330"),("Connection","Keep-Alive"),("project","grupo"),("subject","proposal2"),("email","Alberto")]

pgruponotfound=[("op","vor"),("email","Alberto"),("pass",""),("val","login"),("pass2",""),("backto","modifySubject"),("op","sub"),("type","create"),("REQUEST_METHOD","GET"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING","op=sub&type=create"),("Accept","image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*"),("Referer","http://magovir/FreeChooser.cgi?project=grupo"),("Accept-Language","en-US"),("UA-CPU","x86"),("Accept-Encoding","gzip, deflate"),("If-Modified-Since","Sun, 28 Dec 2008 19:26:11 GMT"),("User-Agent","Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)"),("Host","magovir"),("Connection","Keep-Alive"),("Cookie","project=Grupo"),("project","Grupo"),("REQUEST_METHOD","POST"),("SCRIPT_NAME","/FreeChooser.cgi"),("QUERY_STRING",""),("Accept","image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*"),("Referer","http://magovir/FreeChooser.cgi?op=sub&type=create"),("Accept-Language","en-US"),("Content-Type","application/x-www-form-urlencoded"),("UA-CPU","x86"),("Accept-Encoding","gzip, deflate"),("User-Agent","Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)"),("Host","magovir"),("Content-Length","727"),("Connection","Keep-Alive"),("Cache-Control","no-cache"),("Cookie","project=Grupo"),("project","Grupo")]