{-# OPTIONS -fglasgow-exts #-}
module HTTPParser2 where
import Data.Char ( ord, chr, toUpper, isDigit, isAlphaNum, isHexDigit, isSpace )
--import System.Environment ( getEnv )
import Control.Monad(MonadPlus(..), guard)
import Data.Maybe(fromMaybe)
import System.IO.Unsafe(unsafePerformIO)
import HSP (HSP,XML,renderXML,evalHSP)

e->>a=  fromMaybe "" (lookup a  e)		--Added
infixr 9 ->>
       


-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-Parser]{Yet another combinator parser library}
-- %*                                                                         *
-- %***************************************************************************

-- NOTE: This is all a little bit of a sledgehammer here for the simple task
-- at hand...

-- The parser monad


newtype Parser a = Parser (String -> [(a,String)])

instance Functor Parser where
   -- map :: (a -> b) -> (Parser a -> Parser b)
   fmap f (Parser p) = Parser (\inp -> [(f v, out) | (v, out) <- p inp])

instance Monad Parser where
   -- return :: a -> Parser a
   return v = Parser (\inp -> [(v,inp)])

   -- >>= :: Parser a -> (a -> Parser b) -> Parser b
   (Parser p) >>= f = Parser (\inp -> concat [papply (f v) out
                                             | (v,out) <- p inp])

instance MonadPlus Parser where
   -- zero :: Parser a
   mzero = Parser (\_ -> [])
   -- (++) :: Parser a -> Parser a -> Parser a
   (Parser p) `mplus` (Parser q) = Parser (\inp -> (p inp ++ q inp))
       

-- Other primitive parser combinators

       
item :: Parser Char
item = Parser (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

force :: Parser a -> Parser a
force (Parser p) = Parser (\inp -> let x = p inp in
                             (fst (head x), snd (head x)) : tail x)

first :: Parser a -> Parser a
first (Parser p) = Parser (\inp -> case p inp of
                            []    -> []
                            (x:_) -> [x])

papply :: Parser a -> String -> [(a,String)]
papply (Parser p) inp = p inp
       

-- Derived combinators

       
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `mplus` q)

sat :: (Char -> Bool) -> Parser Char
sat p = do {x <- item; guard (p x); return x}

many :: Parser a -> Parser [a]
many p = force (many1 p +++ return [])

many1 :: Parser a -> Parser [a]
many1 p = do {x <- p; xs <- many p; return (x:xs)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x  <- p
                    xs <- many (do {sep; p})
                    return(x:xs)

char :: Char -> Parser Char
char x = sat (x==)

alphanum :: Parser Char
alphanum = sat (\c -> isAlphaNum c || c == '@' || c =='\'' )    -- Added @ as a valid character

string :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

hexdigit :: Parser Char
hexdigit = sat isHexDigit
       

-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-MIME]{MIME types}
-- %*                                                                         *
-- %***************************************************************************

-- A type a is in class Mime whenever it can be parsed and unparsed
-- and moreover it has a method mimeType for printing its MIME type.

-- This is a VERY naive view of MIME types. Read the documentation for
-- more excitement: http://andrew2.andrew.cmu.edu/rfc/rfc1521.html.

-- However, all PERL libraries I have seen so far are even more
-- primitive. They just dump raw data on stdout.

       
class (Show a, Read a) => Mime a where
   mimeType :: a -> String

   
   
data RawMime = RawMime String String

instance Read RawMime where
   readsPrec _ s = [(RawMime "none" s,"")]

instance Show RawMime where
   show(RawMime _ s) =  s

instance Mime RawMime where
   mimeType (RawMime t _) = t



data XmlResp a= XmlResp a

instance (Show a) => Mime (XmlResp a) where
  mimeType _ = "text/XML"
  
instance Read (XmlResp a) where
	readsPrec _= error "read XML"


instance (Show a) => Show (XmlResp a) where
	show (XmlResp a)= show a 


-- For Haskell Server Pages


instance Mime (HSP XML) where
   mimeType _ = "text/html"


instance Read (HSP XML) where
	readsPrec _= error "read XML"


instance Show (HSP XML) where
	show = renderXML . unsafePerformIO . evalHSP




-- Common type declarations


       
type Name  = String
type Value = String
type URL   = String
       




-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-HTTP-Resp]{HTTP Responses}
-- %*                                                                         *
-- %***************************************************************************

-- The output of a CGI script is a HTTP response (part of, the server
-- adds additional noise), which is either some MIME content, a
-- redirection to some other location, or an error status.

-- We make CgiOut an instance of the Show class so that we don't have to
-- worry about the exact format of the response that is required by the
-- HTTP definition.

-- I think what we really want is to use an existential type here!

-- data CgiOut = Mime a => Content{content :: a}
--            | ....

-- We are not interested in the content type, we only need to know it is
-- in the Mime class.

-- What is missing is a table of (StatusCode,Reason)-pairs such as
-- (503, "server busy") and (200, "Transaction ok") etc.


type StatusCode = Int
type Reason = String
type CName = String
type CValue = String
type CPath = String
type CExpires = String

data Mime a => CgiOut a =
     Content{mime :: a, cookies :: [(CName,CValue,CPath,CExpires)]}
   | Location{url :: URL}
   | Status{status :: StatusCode, reason :: Reason}



showCookies [] = showString ""
showCookies ((n,v,p,e):cs) = 
  showString "Set-Cookie: " . showString n . showString "=" . showString v
   . showString "; path=" . showString p . showExpires e . showString "\n" 
   . showCookies cs

showExpires [] = showString ""
showExpires e  = showString "; expires=" . showString e

instance Mime a => Show (CgiOut a) where
   showsPrec _ Content{mime=mime, cookies=cookies}
    = showString "Content-type: " . showString (mimeType mime)
    . showString "\n"   
    . showCookies cookies
    . showString "\n"
    . shows mime

   showsPrec _ Location{url=url}
    = showString "Location: " . showString url
    . showString "\n\n"

   showsPrec _ Status{status=status,reason=reason}
    = showString "Status: "
    . shows status . showString " " . showString reason
    . showString "\n\n"


-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-Decode]{Decoding application/x-www-form-urlencoded data}
-- %*                                                                         *
-- %***************************************************************************

-- The MIME type application/x-www-form-urlencoded is used to encode
-- tables of (name, value)-pairs that are transmitted from the client to
-- the server.


newtype ApplicationX_Www_Form_UrlEncoded
   = URLEncoded [(String,String)]

instance Read ApplicationX_Www_Form_UrlEncoded where
   readsPrec _ e = papply (do{f <- readEnv; return (URLEncoded f)}) e

instance Show ApplicationX_Www_Form_UrlEncoded where
   show e = "error: show form"


instance Mime ApplicationX_Www_Form_UrlEncoded where
   mimeType _ = "application/x-www-form-urlencoded"


-- An URL encoded value consist of a sequence of
-- zero or more name "=" value pairs separated by "&"

-- Env ::= [Name "=" Value {"&" Name "=" Value}]

-- Names and values are URL-encoded,
-- according to the following table

--   character | encoding
--   ----------|---------
--    ' '      | '+'
--    '<'      | "%XX"
--     c       | "%"hexval(ord c)


urlDecode :: String -> [(Name,Value)]
urlDecode s = case readsPrec 0 s of
                 [] -> []
                 ((URLEncoded e,_):_) -> e

readEnv :: Parser [(Name,Value)]
readEnv = (do 
          n <- urlEncoded
          string "="
          v <- urlEncoded
          return (n,v)) `sepby` (string "&")

urlEncoded :: Parser String
urlEncoded
 = many ( alphanum `mplus` extra `mplus` safe
         `mplus` do{ char '+' ; return ' '}
         `mplus` do{ char '%'
                   ; d <- hexadecimal
                   ; return $ chr (hex2int d)
                   }
         )

extra :: Parser Char
extra = sat (`elem` "!*'(),")

safe :: Parser Char
safe = sat (`elem` "$-_.")

hexadecimal :: Parser HexString
hexadecimal = do d1 <- hexdigit
                 d2 <- hexdigit
                 return [d1,d2]

type HexString = String

hex2int :: HexString -> Int
hex2int ds = foldl (\n d -> n*16+d) 0 (map (toInt . toUpper) ds)
   where toInt d | isDigit d    =  ord d - ord '0'
         toInt d | isHexDigit d = (ord d - ord 'A') + 10
         toInt d                = error ("hex2int: illegal hex digit " ++ [d])






-- A function to do URL encoding and proving its correctness might be a
-- nice exercise for the book.

-- We don't usually need it for CGI scripts though. The browser does the
-- encoding and the CGI script does the decoding.




{-

myGetEnv :: String -> IO String
myGetEnv v = catch (getEnv v) (const (return ""))
                      
getQueryString :: IO String
getQueryString = do
   method <- myGetEnv "REQUEST_METHOD"
   case method of
      "POST" -> do len <- myGetEnv "CONTENT_LENGTH"
                   inp <- getContents
		   return (take (read len) inp)
		  

      _      -> myGetEnv "QUERY_STRING"



getCookieVars :: IO [(Name,Value)]
getCookieVars = do 
  cookies <- myGetEnv "HTTP_COOKIE"
  return (splitCookies cookies)

-}
splitCookies cookies = f cookies []
  where
    f [] r = r
    f xs0 r =
      let xs   = dropWhile (==' ') xs0
	  name = takeWhile (/='=') xs
          xs1  = dropWhile (/='=') xs
          xs2  = dropWhile (=='=') xs1
          val  = takeWhile (/=';') xs2
          xs3  = dropWhile (/=';') xs2
          xs4  = dropWhile (==';') xs3
          xs5  = dropWhile (==' ') xs4
      in  f xs5 ((name,val):r)



