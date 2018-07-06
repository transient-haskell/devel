{-# OPTIONS -fglasgow-exts #-}
module Aprobal(reorderProposalAmends, sumVotes) where

import Data.Maybe(fromJust,catMaybes)
import Data.List(find, maximumBy,nub)
import Data.Array.Diff
import System.IO.Unsafe
import System.Time
import System.Directory(renameFile)
import Control.Concurrent
import Data
import Data.List(sortBy)
import Vote
import VCache
import Workflow(getMajorities, getMajorities1)


{-
para que una enmienda sea aprobada, 
1       si la propuesta ha sido aprobada, el porcentaje de votos debe ser mayor que el asignado a esa categoría
2       Si la propuesta no ha sido aprobada, proceder como sigue
        el objetivo del proceso de enmiendas es:
        que finalmente se apruebe el texto que tenga mas votos positivos y menos compaints
        que el texto final sea independiente de como fuera la propuesta original
        para ello 
                la propuesta original tiene que ser una enmienda mas
                se vota cada una de las enmiendas, incluido el original
                la enmienda mas votada pasa a ser el original
                asi hasta que acaba el proceso de enmiendas
        
                Al acabar el proceso de enmiendas, 
                        si la propuesta Resultante es aprobada entonces pasar a 1
                        si la propuesta Resultante es rechazada se abre otro periodo de enmiendas : pasar a 2

-}



-- check the aprobal of the subject

checkApprobal1:: String -> Subject  -> Subject
checkApprobal1 project sub  
    | sstatus sub /= Processing = error ("checkApprobal: proposal"++sname sub ++ "was not under votation")
    | percentComplaintThis  > fromIntegral (percentComplaint majorities )
                    = sub{sstatus = Rejected (Unconstitutional percentComplaintThis)}
    | sumvotes < fromIntegral (percentNecessary majorities)
                    = sub{sstatus = Rejected (NotEnoughVotes sumvotes)} 
    | otherwise=defaultCheck
                {-
                case (checkApprobal prConf)  of
                    Nothing -> defaultCheck
                    Just f  -> f sub majorities
                -}
    where

    majorities= getMajorities1  sub
    percents = getPercents $ sumVotes  sub    
    sumvotes= 100 - percents ! 0
    lpercents= elems percents
    noptions= length lpercents -2
    percentComplaintThis=   percents ! last where (_,last)= bounds percents 
    percentDesapprovedThis= percents ! 2
    percentApprobedThis= percents ! 1
    defaultCheck=
       case (options sub)of   
        Approbal ->                         -- yes/no votations
          if percentApprobedThis > fromIntegral (percentAprobal majorities) 
                then  sub{sstatus=Approbed percentApprobedThis }
          else sub{sstatus= Rejected (NegativeVote percentDesapprovedThis )}

        ChooseOptions question n opts ->    -- votation for many options
          sub{sstatus= Approbed 0, 
              options= ChooseOptions question n ((select n) $ sortOpts ( zip opts (tail lpercents)))}
       where
       
       sortOpts = sortBy cmp 
       cmp (op,pc) (op',pc')= compare pc pc'
       select noptions (((Option str _),votes):rs) = Option str (Approbed votes): select (noptions-1) rs
 


-- call before visualize 

-- check the approbal of the subject and the amends

reorderProposalAmends project subject= do
        Rs s <- justGetVResource $! Rs uSubject{sname=subject, prosname= project}
        maj<- getMajorities (category s)  project
        amends<- getAmends project $ sname s
        checkAmends s amends maj

--check the approbal of the amends
-- choose the amed with more positive votes that is constitutional
--  constitutional means that the amend  has'nt enough complaints to be rejected

checkAmends s amends majorities= do
        -- get the ammend with most votes and no rejection
        list<- mapM (checkAmend (prosname s)(sname s)) amends
        let (amd,percent)= case (catMaybes list) of  -- use sort instead o maximum
                            [] ->  (uSubject,0)
                            l  ->  maximumBy (\(_,p) (_,p')->compare p p') l
        
        case (options s)of
         Approbal ->
          case (sstatus s) of
                Approbed _  ->  error $ "checking an approbed proposal: "++show s

                Processing  ->  if percent > (getPercents $ sumVotes  s) ! 1  then changeSubject s amd
                                else return()

                _           ->  return()

         ChooseOptions _ _ _->
          case (sstatus s) of
                Approbed  _ ->  {-return()-} error $ "checking an approbed proposal: "++show s


                Processing  ->  if percent > fromIntegral(percentAprobal majorities)  then changeSubject s amd
                                else return()
  where
        checkAmend project subject amend= do
                Ra  amd <- justGetVResource $! Ra  uSubject{aname=amend,sname=subject,prosname= project}
                let percents = getPercents $ sumVotes amd
                let (_,last) = bounds percents
                if percents ! last < fromIntegral ( percentComplaint majorities) 
                 then return$ Just (amd, percents ! 1)
                 else return Nothing
                
                

numOptions Approbal= 4
numOptions (ChooseOptions _ _ l) = length l

{-- accumulate votes by option

sumVotes:: Array Int PriorIUser -> Int -> Array Int Int
sumVotes votes len = hist   where
        list= elems votes
        hist  = accumArray (+) 0 (0,len) $ map (\(_,xs)-> (xs,1)) list

-}    




-- get the percentages of each option
getPercents:: DiffUArray Int Int -> Array Int Float
getPercents svotes= array b [(i,percent1 (svotes!i))|i<-range b] where
        b= bounds svotes
        percent1 x|  allVotes==0 = 0.0 :: Float
                  | otherwise= (fromIntegral x*100)/(fromIntegral allVotes) 
        
        allVotes=  sum $ elems svotes

-- If a amend has more votes than the subject when the subject has´nt reach the official votation start
-- or else, after subject approbal, if the amend possitive votes has enough votes for his category then
-- change the subject by the amend, and the subject becomes a amend.

changeSubject s amd= do  
        withVResources[Rs s,Ra  amd, uRl{lname=amsname} ] change
        deleteVResource (Ra  amd)
        return () 

  where

        amsname= listAmendsNamePs project subject
        project= prosname s
        subject= sname s
        change[Just(Rs s),Just (Ra  amd),Just (Rl _ amends)]= [Rs s',Ra  amd',Rl amsname amends' ] where
            amends'= subst amends (aname amd) nname 
            s'=s{   category= category amd
                    ,authors= nub $ authors s++authors amd
                    ,topics = topics amd
                    ,content= applyChan         
                    ,options = options amd
                    ,votes= votes amd
                    ,lastVote= lastVote amd
                    ,daysBefore= daysBefore amd
                }
            amd'= diffAmend s' s{aname=nname{-++ show now-}, votes= votes s} 
            nname= newname initname where
                initname= "previous proposal derogated"
                newname n | n /= aname amd= n 
                          | otherwise= newname (n++" I")
                           
        change _= error $ "changing subject: " ++ show s    
        
        contents s = str where Str str= content s

        applyChan= case (content amd) of
                                Changes ch ->   Str $ concatMap(\s->s++"\n") $ applyChanges (lines1 $ contents s) ch
                                _          ->   content amd
                        
        
-------------------------------------------------------------------------------------------
{---------------------------------------tasks for aprobal and close proposals, and open Proposals for vote again---------------     
cronList ::[(CronOp,[String]->IO())]

cronList =[(CloseProposal,closeProposal)
          ,(CheckApprobalProp,checkApprobalProp)
          ,(OpenPropForVote,openPropForVote)]

startCronTab= do
    jcrtab<- getVResource $ Rt cronTab undefined
    case jcrtab of
        Nothing -> return()
        Just (Rt _ l) -> mapM_ cronExec l


    
cronExec:: (Integer, CronOp, [String]) -> IO ThreadId
cronExec (t,fname,args)= forkIO $ do
            threadDelayInteger $ delay* 1000000 
                
            print ("execution at tnow="++show tnow++" ,t="++show t)
            (fromJust $ lookup fname cronList) args
    where
        delay | t-tnow >0 = t-tnow 
              | otherwise  = 0
              
        threadDelayInteger:: Integer -> IO()
        threadDelayInteger time| time < imaxInt = threadDelay $ fromIntegral time
                               | otherwise=do   threadDelay maxInt
                                                threadDelayInteger ( time - imaxInt)
            where   maxInt = maxBound :: Int
                    imaxInt= fromIntegral maxInt
                   



addSubjectProcs:: Integer->Integer-> Subject-> Maybe ResourceVote-> ResourceVote
addSubjectProcs t1 t2 s mplist= (unsafePerformIO $ mapM cronExec procs) `seq`
  case mplist of
    Just(Rt _ plist) -> Rt cronTab (procs++plist)
    Nothing ->          Rt cronTab procs
  where
        procs=  [(t1,"checkApprobalProp",[project, subject, scategory]),(t2, "closeProposal", [project,subject,scategory])]
        project= prosname s
        subject= sname s
        scategory= show $ category s
 
        
addSubjectProc:: Integer-> CronOp -> Subject-> Maybe ResourceVote-> ResourceVote
addSubjectProc t1 name s mplist= (unsafePerformIO $ cronExec proc) `seq`
  case mplist of
    Just(Rt _ plist) -> Rt cronTab (proc:plist)
    Nothing ->          Rt cronTab [proc]
  where
        proc=  (t1,name,[project, subject, scategory])
        project= prosname s
        subject= sname s
        scategory= category s
 
checkApprobalProp args@[project,subject,category]=do
    majorities<- getMajorities category project 
    withVResources [Rs uSubject{sname=subject,prosname=project}
                   ,uRt{tname=cronTab}] $ doIt majorities
    delCronTab  (undefined,CheckApprobalProp,args)  --delete task from task list 
    
  where
    doIt _ [Nothing,_]= error ("Proposal not found in database: "++subject)
    doIt majorities [Just (Rs s),cronList]= 
      case (sstatus s,cronList) of
          (Approbed _,_) -> [Rs s',cronList']
          _              -> [Rs s']
      where
          s'= checkApprobal1 project s 
          t1= fromIntegral.timeSpan.unsafePerformIO $ getMajorities category  project
          cronList'= addSubjectProc t1 CloseProposal s cronList 
               
    
    -}
        
closeProposal args@[project,subject,category]= do
    maj<- getMajorities  category project 
    let vtime=  votationTime maj
    let span = fromIntegral $ timeSpan maj

    withVResources [Rs uSubject{sname=subject,prosname= project}]
                   --,uRt{tname=cronTab}] 
    		   $ doIt vtime span scategory
    --delCronTab  (undefined,CloseProposal,args) --delete task from task list
    
    where
    scategory= read category
    doIt _ _ _ [Nothing]= error ("Proposal not found in database: "++subject)
    doIt vtime span scategory [Just (Rs s)]= 
                     case scategory of
                        "Constitutional" -> 
                        		    [Rs s{lastVote= flastVote $ fromIntegral vtime}]
                        		    --,addSubjectProc span  OpenPropForVote s crtab]
                        _                -> [Rs s{sstatus= Closed $ sstatus s}] 
                                        
openPropForVote args@[project,subject,category] = do
  withVResources [Rs uSubject{sname=subject,prosname=project}]
                 --,uRt{tname=cronTab}] 
                 $ openIt
  --delCronTab (undefined,OpenPropForVote,args)
  where
  time= fromIntegral.votationTime.unsafePerformIO $ getMajorities category  project
  openIt [Just (Rs sub)]= 
      [Rs sub{sstatus= Processing} ]
      --,addSubjectProc time CheckApprobalProp sub lprocs ]
        
  openit [Nothing] = error subject++" not found in openProfForVote"

