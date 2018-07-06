module WorkflowElem where
import Data.List(elemIndex)
import Data.Maybe(fromJust, isNothing)
import Control.Concurrent(threadDelay)
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Workflow
import Data.TCache.Dynamic (refcache,keyResource)
import Data
import Vote
import VCache
import Aprobal (checkApprobal1,getMajorities1)



------------

--criteria proposal -> devuelve la serie de acciones que aplican a una propuesta para un usuario dado.

--como un grupo es un miembro juridico con un voto en otro?
--data User= User1 | UserGroup String 

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

execWorkflow :: ResourceVote ->  IO ResourceVote
execWorkflow  rs@(Rs sub)= do
       ml <- getVResource (Rwf undefined)
       let nwflist= if isNothing ml then [] else l where Rwf l= fromJust ml
       let key= prosname sub ++"."++ category sub
       f <- case lookup key nwflist of
              Nothing-> do
                   Rp pr <- justGetVResource $ Rp uProject{pname= prosname sub}
                   case lookup (category sub) (propTypes pr) of
                      Nothing -> error $ "category not found in project "++ prosname sub
                      Just lambda@(Lambda _ (Workf f)) -> do
                         let doit (Just(Rwf list)) = Rwf $  (key, lambda):list
                             doit Nothing =          Rwf $ [(key, lambda)]
                         withVResource (Rwf undefined) doit 
                         return f
              Just (Lambda _ (Workf f)) -> return f 

       startWF key rs [(key,f context)] 

addWfFunc key lambda@(Lambda _ (Workf f))= do
       ml <- getVResource (Rwf undefined)
       let nwflist= if  isNothing  ml then [] else let Rwf l= fromJust ml in l
       case lookup key nwflist of
              Nothing -> do
                let nnwflist= (key, lambda):nwflist
                addVResource $ Rwf nnwflist
                return f
              Just (Lambda _ (Workf f)) -> return f 

stepAct act e= (step (action act )) e
context  = stepAct

getActions ::  ResourceVote ->  [String]
getActions (Rs sub)=
    case actionParams sub of
             VoteDefault _  True ->["link a vote","link a amend"]
             VoteDefault _  _ ->["link a vote"]
             Vote  _ ->["link a vote"]
             AmendDefault _ ->["link a amend"]
             Amend _ ->["link a amend"]
             Execute _ -> ["view"]
             _ ->  error $ "No Action in Subject "++show sub
      

              
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



action ::  Action -> ResourceVote -> IO  ResourceVote
action  proc e1@(Rs sub1)= do
  logEntry $ "Action <"++ show proc ++ "> for "++ keyResource e1
  let e@(Rs sub)= Rs sub1{actionParams=proc}
  case proc of
            Propose -> return e
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
            

            WaitFor (Lambda _ (Filter f)) -> waitFor (f context) e
           
            StartWorkflow lambda@(Lambda _ (Workf wf)) -> do
                        let key= keyResource e
                        fl<- addWfFunc key lambda
                        startWF key e [(key,fl context)]
                        
            
            VoteDefault maj amend -> do 
	              let maj= getMajorities1 sub 
                      let daysb = votationTime maj
                      let last=  flastVote $ fromIntegral daysb 
                      let sub1= sub{daysBefore=daysb, lastVote= last } 
                      addVResource $ Rs  sub1
                      waitUntil last
                      return $ Rs $ checkApprobal1 sub1
                      
                      
           


