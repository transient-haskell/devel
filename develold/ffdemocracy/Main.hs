{-# OPTIONS   -XOverloadedStrings #-}
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.String

main= runNavigation "" $ step $ do
  pag <- getRestParam `onNothing` return "Index"
  ask $(docTypeHtml $ do
       El.head $ do
        El.title $ fromString pag)
        ++> (El.div ! At.style "float:right" <<< autoRefresh wlogin )
        **> tFieldEd "editor" (wikip ++ pag ++ "body.html") "Enter the body"
        **> noWidget

wikip= "wiki/"

