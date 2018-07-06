{-# LANGUAGE NoMonomorphismRestriction, CPP #-}
import Prelude hiding (id)
import GHCJS.HPlay.View
import Transient.Base
import Transient.Move
import Transient.Move.Utils
import Control.Monad.IO.Class 
import Data.String
import Data.Monoid


#ifdef ghcjs_HOST_OS
import GHCJS.Foreign
import Data.JSString
#endif

fs= fromString



main =  do
#ifdef ghcjs_HOST_OS
    addHeader $ do
        link ! atr (fs "rel") (fs "stylesheet")
                      ! href (fs "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.css") 
        script $ "var mde;"
        script ! src (fs "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.js") $ noHtml
        script $   "function waitmde(){if (typeof SimpleMDE !== 'undefined') {mde=new SimpleMDE();}else{window.setTimeout(function(){waitmde();}, 10);}};waitmde()"

#endif 
    keep $ initNode $ do
            

        newContent <- local . render $  do
                    textArea  (fs "hello")  `fire` OnKeyUp
                    liftIO getMDEcontent 
                        
        result <- atRemote  . localIO $ do
                       print newContent
                       return $ newContent <> fs " received by the Server"

        local . render $ rawHtml $ p result

#ifdef ghcjs_HOST_OS
-- it is "mde.value()"  not "mde.value();" which produces "undefined"   this was aprox.. 5 hours of debugging
foreign import javascript unsafe  "mde.value()" getMDEcontent ::  IO JSString
#else
getMDEcontent= error "not defined on server" :: IO String
#endif