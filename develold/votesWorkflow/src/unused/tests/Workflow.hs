module Workflow where
import Data.List(elemIndex)
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Workflow
import Data.TCache.Dynamic (refcache,keyResource)
import Data
import Vote
import VCache




--criteria proposal -> devuelve la serie de acciones que aplican a una propuesta para un usuario dado.

--como un grupo es un miembro juridico con un voto en otro?
data User= User1 | UserGroup String 

{-
con el implicit workflow:
  [Step "remitir" groupname remite]
  
y el workflow de grupo remitido
  [Step "vote" "this" vote, Step "remitir" grupoinicial remite resultado]
  luego workflow puede remitir no solo proposals, sino cualquier otro valor? 
  
Subject necesita dos campos: estado de flujo. 
Como Amend, el contenido puede ser un subject, asi un Subject puede tener infinitas recurrencias


cada paso del workflow es un estado en el subject. a cada paso, o bien se realiza un proceso automático (enviar a otro grupo por
ejemplo o bien se espera a ser procesado manualmente.
normalmente proceso manual -> action -> proceso manual -> action
se debe notificar explicitamente el grupo en el workflow o se debe poner como una acción genérica?
componentes de un paso: grupo que lo hace, nombre de la acción. De la forma mas sencilla:

-}

   
getMajorities :: String -> String  -> IO Majorities
getMajorities subject project = do
  Rs sub <- justGetVResource $ Rs uSubject{sname=subject, prosname=project}
  return $ getMajorities1 sub

getMajorities1 sub=
  case actionParams sub of
    Action (VoteDefault maj _) _-> maj
    _    -> error "no majorities"

execWorkflow :: Elem ->  IO Elem
execWorkflow  rs@(Rs sub)= do
    let Rp pr = unsafePerformIO $ justGetVResource $ Rp uProject{pname= prosname sub}
    case lookup (category sub) (propTypes pr) of
        Nothing -> error $ "category not found in project "++ prosname sub
        Just (Lambda _ (Workf f)) -> do
              let key= keyResource rs
              startWF key rs [(key,f context)]

stepAct act= step (action act)
context  = (stepAct, refcache)

getActions ::  Elem ->  [String]
getActions (Rs sub)=
    case actionParams sub of
       Process _ -> error $ "No Action in Subject "++show sub
       Action act _ ->
          case act  of
             Propose      -> [] 
             VoteDefault _  True ->["link a vote","link a amend"]
             VoteDefault _  _ ->["link a vote"]
             Vote  _ ->["link a vote"]
             AmendDefault  ->["link a amend"]
             Amend _ ->["link a amend"]
             Execute _ -> ["view"]

              
{-
en ese grupo
substmembers: Workflow
substmenbers: Action Exec (\prop -> ...

como añadir demas parametros: tipo de propuestas y porcentajes y asociarlos con workflow?
-}


{-
Interpretación de workflow:
Propose significa que este tipo de propuestas pueden ser creadas dentro del grupo
El Elemento pasa a un estado siguiente, el presentador de elementos ya definido, genera los verbos disponibles para cada elemento
Solo hay una opción disponible en cada momento.
deberia poder parametrizarse las mayorias y los tiempos en los workflows y tambien las funciones a ejecutar si no son las de defecto.
-}



action ::  Action -> Elem -> IO  Elem
action (Process proc) e@(Rs sub)= 
  case proc of
            Remite gr wf  ->case gr of
                 Group gr-> do
                            let sub=  blankSubject{sname= sname sub,category=show wf, prosname= gr,content= Res $ Rs sub}
                            execWorkflow $ Rs sub
                            return (Rs sub)
                 Sender  -> do
                            let Res elem= content sub
                            return elem

            Create e  ->  return e

            Exec (Lambda _ (ExecFunc f)) ->   f context e
            

action  (Action  act follow) elem = do
           
           let filter = \e-> foldr (||) False [f context e | (Lambda _ (Filter f),_) <- follow]
 
           elem' <- waitFor filter elem
           
           doOneOf follow elem'
    where      
    doOneOf [] e= return e
    doOneOf ((Lambda _ (Filter f),Lambda _ (Workf wf)):ws) e= if f context e then startWF key e [(key,wf context)]  else doOneOf ws e
        where
        key= keyResource e
           
           


