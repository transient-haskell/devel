
data Tag= Tag {[attributes :: [String], tags ::  [Tag]]}

type XML= Tag

data Val= ValS String | ValT Tag

xml= unsaferPerformIO $ takeMVar mvxml

tag :: String -> Tag
tag s= tag1 x xml
   where
   tag1 s tag=
      case lookup "name" xml of
        Just s -> xml
        _      -> head $ map (tag1 s) $ tags xml


attribute tag atr= lookup str $ attributes tag

a . b= b a

tag "a"  . tag "b"  . attibute "c"
