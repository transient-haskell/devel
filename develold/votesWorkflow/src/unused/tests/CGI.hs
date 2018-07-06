module CGI where
 
data CGI = CGI { 
	valueOf :: [(String,String)] -> IO String 
}
