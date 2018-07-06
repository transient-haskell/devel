module CGI2 where
 
data CGI2 = CGI2 { 
	valueOf :: [(String,String)] -> IO String 
}
