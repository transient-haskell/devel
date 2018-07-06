module URIHold where
import Network.URI
import Data.List (elemIndex)
data URIHold=  URIHold String URI deriving Eq

instance Read URIHold where
  readsPrec _ str= case parseURI str2 of
     Just uri -> [(URIHold str2 uri,str3)]
     Nothing  -> error $ "error parsing uri:"++ str2++"\""
     where
     strsp= dropWhile (==' ') str
     (str2,str3)= break (\c -> c== ' ' || c ==',') strsp   



instance Show URIHold where
    show (URIHold str _)=  str


genURI :: String -> Maybe URIHold
genURI url= do
    uri <- parseURI url
    return $ URIHold url uri
