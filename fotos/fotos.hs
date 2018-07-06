#!/usr/bin/env execthirdline.sh
-- compile it with ghcjs and  execute it with runghc
-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient  bash -c "cd /work && mkdir -p ./static && ghcjs -itransient/src -itransient-universe/src  -iaxiom/src ${4} ${1} -o static/out && runghc -itransient/src -itransient-universe/src -iaxiom/src  ${4} ${1}  ${2} ${3}"

{-# LANGUAGE NoMonomorphismRestriction, CPP #-}
module Main where
import Transient.Base
import Transient.Move

import GHCJS.HPlay.View as V hiding (map,head)
import Data.String
import Data.Function ((&))
import Data.List
import Data.Maybe
import Prelude hiding (div,id)
import Data.IORef
import Control.Monad.IO.Class
import Data.Typeable
import Control.Monad(when)
import System.Directory

data Format= Portrait | Landscape
type Name= String
type Desc= String 
type PhotoFile= String 
type Projects = [(Name,Desc,[PhotoFile],Format)]

projects :: Projects
projects=
#include "content.txt"

type Style=  String
type Project = Int
type Photo= Int
type OnDisplay= (Project,Photo, Style)
data Current= Current OnDisplay



files = "files"


filterdir= return . filter ((/= '.') . head)

fs= fromString
main=  do
  -- when compiled with ghc, will generate the "content" by reading the content of "files"
#ifndef ghcjs_HOST_OS
--  -- Just generates the "content" file
--  prs <- liftIO $ getDirectoryContents files >>= filterdir
--  projects <- liftIO $ mapM (\f -> (getDirectoryContents $ files ++ "/" ++ f) >>= filterdir)  prs
--  liftIO $ writeFile "content" $ "\t"++ (show $ zip3 (reverse prs) "" projects)

    keep $ initNode (empty :: Cloud ())  -- a web server for tests (files to serve must be in the folder "./static/out.jsexe")
#else

    runBody $ Widget $  do
        setRData $ Current (0,0,"")
        -- render $ (iframe ! id (fs "content") ! src (fs "content") $ noHtml) `pass` OnLoad
        -- liftIO $ alert $ toJSString "JJJJ"
        -- content <- liftIO $  js_getContent >>=  fromJSVal
        -- liftIO $ alert $ toJSString $ show (content :: Maybe String)

        insertStyles
        panels
        reinitpage <|> chooseProject  <|> bio <|> statement <|> contact <|> initialPhoto

-- foreign import javascript unsafe "document.getElementById('content').innerText"
--      js_getContent  :: IO JSVal  

initialPhoto=  render $ rawHtml $ do
      forElemId (fs "gallery") $ this ! clas (fs "landscape") `child` do
                    clear
                    let  proj= projects !! 0
                         (image,_)= break (==' ') $ (proj & trd) !! 1
                    img ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ image)
                        ! style (fs "width:70%")
      forElemId (fs "nav")  clear         

insertStyles=
   liftIO $ addHeader $ link ! atr (fs "rel") (fs "stylesheet")
                             ! href (fs "fotos.css") 

-- | the skeleton of the app
panels= do
   render $ rawHtml $ do
         h2 ! id (fs "init") ! style (fs "color:black;margin-bottom:0px;font-weight:700;font-size:20px") $  "MARIA TORIJA"
         h4 ! style (fs "margin-top:0px") $ "Photography"

         div ! id (fs "leftpane") ! clas (fs "leftpane")$ do
               h3 ! style (fs "color:black") ! id (fs "works") $ "Works"
               div ! id (fs "projects") $ noHtml
               br
               br
               h3 ! style (fs "color:black") ! id (fs "bio") $ "Bio"
               h3 ! style (fs "color:black") ! id (fs "statement") $ "Statement"
               h3 ! style (fs "color:black") ! id (fs "contact") $ "Contact"

               br
         div ! id (fs "gallery") ! clas (fs "portrait") $ noHtml
         div ! id (fs "nav") $ noHtml
         div ! clas (fs "copyright")  $ "© Maria Torija 2017"


bio=  do
         render $ at (fs "#bio")  Insert $ wlink "bio"  (fs "Bio")  ! style (fs "color:black")
         Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )
         render $ rawHtml $ do
                     forElemId (fs $ fst' (projects !! n))   $ this ! clas (fs "other")
                     forElemId (fs "nav")  clear
                     forElemId (fs "gallery")  $ this ! clas (fs "landscape")  `child`  clear >> biotext  
         return ()
      where 
      biotext= do
        div $ do
            img ! style (fs "width:20%;height:20%;float:left;margin-right:5%") ! src (fs "bio.jpg")

            p ! atr "align" (fs "justify") $
               "Puedo decir que me considero Madrileña. Soy licenciada en Veterinaria y Derecho, mi trayectoria laboral se centra en la Administración Publica."

            p ! atr "align" (fs "justify") $
               "Comienzo en la fotografía como autodidacta ,efectúo cursos en diversas escuelas de Fotografía  La Máquina , escuela Blank Paper ,escuela Lens. escuela Pica Photoespaña y Efti. Asistencia a talleres con reconocidos fotógrafos ;Lurdes R, Basoli , Jesús Mico Ricky Dávila , Eduardo Nave, David Jiménez , Laia Abril, Matías Costa ,  Javier Vallhonrat, Aleix Plademunt"

        br
        div ! style (fs "float:left") $ do
            p  ! style (fs "margin: -2% 0 0 -2.5%") $ b $ "PREMIOS" 
            ul ! style (fs "list-style: none;") $ do
                li $ "Finalista 5ª Edición del Festival de Fotografía BFOTO, Barbastro 2018"
                li $ "Seleccionada Transeuropa  PhotoEspaña 2018"
                li $ "Seleccionada PhotoAlicante 2018" 
                li $ "Finalista XVII Seminario  de Fotografia  de Albarracin Teruel 2017" 
                li $ "Seleccionada Descubrimientos PH 17 2017" 
                li $ "Finalista Beca Master BASE  escuela LENS 2017"
            
            p  ! style (fs "margin: -2% 0 0 -2.5%")  $  b $ "EXPOSICIONES" 
            ul ! style (fs "list-style: none;")  $ do
                li   $ "2018 Exposición   colecvtiva 16 CUADERNOS VALLECANOS en CASA MADRID" 
                li   $ "2017 Showroom , exposicion colectiva \"Archipielago\""
             
            p  ! style (fs "margin: -2% 0 0 -2.5%")  $ b $ "PUBLICACIONES" 
            ul ! style (fs "list-style: none;")  $ do
                li  $ "Fotolibro colectivo  Barrios Project , escuela PICA" 

            p  ! style (fs "margin: -2% 0 0 -2.5%")  $ b $ "ACTIVIDADES" 
            ul ! style (fs "list-style: none;")  $ do
                li  $ "Charla \"Conversaciones con Fotógrafas\". Universidad Rey Juan Carlos 2018"
                

statement= do
         render $ at (fs "#statement")  Insert $ wlink "statement"  (fs "Statement")  ! style (fs "color:black")
         Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )
         render $ rawHtml $ do
                    forElemId (fs $ fst' (projects !! n))  $ this ! clas (fs "other") 
                    forElemId (fs "nav")  clear
                    forElemId (fs "gallery")  $  this ! clas (fs "landscape")  `child` clear >>  statext 
         return ()
     where
     statext= div $ do
         p ! atr "align" (fs "justify") $
              "En la elaboración de mis trabajos fotográficos siempre surge esta pregunta ¿ Es posible llegar a compartir la realidad con alguien? Compartir el acto de mirar y la realidad resulta al final una forma de superar la insidia entre la existencia y la verdad, de alcanzar un fundamento vital."
              

         p ! atr "align" (fs "justify") $
              "En mis trabajos personales no muestro fotografías de realidades amables y explicitas , supongo que dentro de mi hay una atracción hacia la oscuridad y lo misterioso, el doble sentido de las cosas y la realidad oculta que no se evidencia, pero se intuye. Tampoco hago fotografías para informar, aunque todo lo que sale en ellas es real, son cosas y personas que están allí, pero vistas desde mi subjetividad documental, me interesan las construcciones sociales que articulan la realidad vital , sus consecuencias en la existencia del hombre."

         p ! atr "align" (fs "justify") $
              "No estoy segura de que todo lo que estoy diciendo ahora sea válido en un futuro próximo, estoy en pleno periodo de aprendizaje y evolución y lo más importante de vivencia fotográfica"

         

contact= do
         render $ at (fs "#contact")  Insert $ wlink "contact"  (fs "Contact")  ! style (fs "color:black")
         Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )
         render $ rawHtml $ do
                    forElemId (fs $ fst' (projects !! n))   $ this ! clas (fs "other") 
                    forElemId (fs "nav")  clear
                    forElemId (fs "gallery")  $ clear >> contactext 
         
         where 
         contactext=  do
               p ! style (fs "text-align:center") $ "María Torija"
               p ! style (fs "font-weight:bold;text-align:center") 
                 $ a ! style (fs "color:black") 
                     ! href (fs "mailto:mariajtalonso@gmail.com") $ "mariajtalonso@gmail.com"

renderGallery= do
   clikableGallery <|>  leftRight
               -- click in the gallery would render the next photo recursively
   renderGallery
   where
   clikableGallery= do
     gallery
     norender forward

reinitpage= do
   render $ at (fs "#init")  Insert (wlink "init"  (fs "MARIA TORIJA")  ! style (fs "color:black")) <|>
            at (fs "#works") Insert (wlink "works" (fs "Works")          ! style (fs "color:black"))
   Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )

   setRData $ Current (0,0,"") 
   render $ rawHtml $ do
       forElemId (fs $ fst' (projects !! n))   $ this ! clas (fs "other")

   initialPhoto


chooseProject= do


    project <- render $ at (fs "#projects") Insert  $ do
                                foldr  (<|>) empty [ wlink project (h4 ! id (fs project) $ project) 
                                                            | (project,txt,_,_) <-  projects]

    Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )

    let n'= fromJust $ findIndex (==project) $ map fst' projects
    setRData $ Current (n',0,"")
    
    let classgal= fs $ case fourth $ projects !! n' of
         Portrait ->  "portrait"
         Landscape -> "landscape"
   
    

    render $ rawHtml $ do
       forElemId (fs "gallery") $ this ! clas  classgal
       forElemId (fs $ fst' (projects !! n))   $ this ! clas (fs "other")
       forElemId (fs $ fst' (projects !! n'))  $ this ! clas (fs "highlighted")

    clicableText n' <|>  staticNav (render $ at (fs "#nav") Insert (wlink () (fs ">>>>")))
    renderGallery
    where
    clicableText n'=  do
       render $ at (fs "#gallery")  Insert $ 
                           (p ! atr "align" (fs "justify")
                             $ snd' (projects !! n'))  `pass`  OnClick
       return ()

fst' (x, _, _,_)= x 
snd' (_, x, _,_)= x
trd  (_, _, x,_)= x
fourth (_,_,_,x)= x


instance Monoid Int where
   mempty= 0
   mappend= (+)

-- style= atr (fs "style")

-- | display the current image. it stop, and continue when the image is clicked (OnClick)
gallery = do
    Current (n,m,classMove) <-  getRData <|> return (Current (0,0,""))

    let proj=(projects !!n)

  -- preload next photo
    let str= ( proj & trd) !! m
        (image,t) = break (==' ') str
        classgal= case t of
         " P" -> "portrait"
         " L" -> "landscape"
         _   -> case  proj & fourth  of
                  Portrait ->  "portrait"
                  Landscape -> "landscape"
--    liftIO $ alert (fs $ show (image, classgal))
    render $ at (fs "#gallery") Insert $ 
      (this `goParent` this ! clas (fs classgal) `child` do
                img ! clas (fs classMove)
                    ! src (fs $ "../"++files++"/"++ (proj & fst') ++ "/"++ image)
                    ! style (fs "width:100%"))
                  `pass` OnClick

    -- when (m < lengthImages n -1) $ render $   
    --     rawHtml $ img ! style (fs "visibility: hidden;width:0px;height:0px")
    --                   ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! (m+1))
 
    return()


lengthImages pro= (projects !! pro) & trd & length

clas= atr (fs "class")
leftst= "w3-animate-left"
rightst= "w3-animate-right"


leftRight= staticNav $  render $ at (fs "#nav") Insert (left  <|> right)
   
left  = do
      Current (n,m,_) <-  Widget $ getRData <|> return (Current (0,0,""))
      if (m > 0) 
       then do
            wlink "left"  ( fs "<<<<")   <++ toElem " | " -- ! id (fs "left") !  clas (fs "w3-btn-floating"
            backward
       else empty


backward = Widget $ do
      Current (n,m,_) <- getRData <|> return (Current (0,0,""))
      let m'= m-1
                  -- if m == 0
                  --      then  let n'= length projects -1
                  --            in if n == 0 then (n',lengthImages n' -1) else (n-1,lengthImages (n-1)-1)
                  --    else (n,m-1)


      setRData $ Current (n,m',leftst)
      -- when (n' /= n) $ changeText n n'
    --   render . rawHtml $ do
    --       when (m == lengthImages n ) $ forElemId (fs "right") $ this ! style (fs "visibility:visible") 
    --       when (m'==0)                $ forElemId (fs "left" ) $ this ! style (fs "visibility:hidden")


right = do
  Current (n,m,_) <-  Widget $ getRData <|> return (Current (0,0,""))
  if (m < lengthImages n -1 )
    then  do
      wlink "right" ( fs ">>>>")     --   ! clas (fs "w3-btn-floating") 
      forward
    else empty

forward =  Widget $ do
    Current (n,m,_) <- getRData <|> return (Current (0,0,rightst))
    let m'= m+1
    -- let (n',m')=  if m == lengthImages n - 1
    --                  then if n == length projects - 1 then (0,0) else (n+1,0)
    --                  else (n, m+1)

    setRData $ Current (n,m',rightst)
    -- when (n' /= n) $ changeText n n'
    -- render . rawHtml $ do
    --      when (m'== lengthImages n )  $ forElemId (fs "right") $ this ! style (fs "visibility:hidden") 
    --      when (m==0)                  $ forElemId (fs "left" ) $ this ! style (fs "visibility:visible")








-- STRefs for the Transient monad

newtype Ref a = Ref (IORef a)

-- | An state reference that can be updated (similar to STRef in the state monad)
--
-- Initialized the first time it is set.
setRData:: Typeable a => a -> TransIO ()
setRData x= do
     Ref ref <- getSData
     liftIO $ atomicModifyIORef ref $ const (x,())
   <|> do
     ref <- liftIO (newIORef x)
     setData $ Ref ref

getRData :: Typeable a => TransIO a
getRData= do
    Ref ref <- getSData
    liftIO $ readIORef ref


#endif
