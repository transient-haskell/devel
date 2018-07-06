{-# OPTIONS -fglasgow-exts #-}
module TestServer where


import Debug.Trace
--import Control.Concurrent
import CGI
import Init
import VCache 
import Data
import Vote    
import Pages
import Lang
import LangEs
import WebServer ((->>))

cgi= CGI cgi1

cgi1 env= do 
  
         initialize1  `debug` "in FreeChooser.hs"
         
 
         logEntry $ show env
         r <- regAction [] "pepe" "3025" "3025"
        
         return ( show r)  `debug` "vuelve"
         

main =do 
     let elem=  Rs blankSubject{sname="hola", prosname="que tal"}
     let doit _= [elem]
     withVResources [] doit
     
     
     Rs sub <- justGetVResource elem
     return sub
     
     


regAction env email pass pass2 = do
    if pass2 /= pass then 
	return $ registrationPage env "The password verification failed"
     else do
	r <- userRegister email pass `debug` "user register"
	case r `debug` "despues" of
		True	-> backTo env
		False	-> return $ registrationPage env temaregistered `debug` "false"
    where
	temaregistered = case env->>acceptLang of
			    "es" -> LangEs.emaregistered
			    _    -> Lang.emaregistered
    
backTo env= case backto `debug` backto of
				"modifyProject"  -> modifyProjectPage env cookies "" `debug` "mod"
				"modifySubject"	 -> modifySubjectPage env cookies "" `debug` "mods"
				_		 -> userPage env cookies tsuccreg `debug` "userpage"

	where 	cookies	= [("email", email, "/", endCookie)]
		backto	= env->>"backto"
		email   = env->>"email"
		tsuccreg= case env->>acceptLang of
			    "es" -> LangEs.succreg
			    _    -> Lang.succreg
