{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Vote where

--import Data.Array.IArray
import Data.Array
import System.Time
import System.Locale
import Data.Typeable
import Data.Char (isAlphaNum,isSpace)
--import Text.XML.HaXml.Haskell2Xml 


import Data.IORef
import Data.List(find, findIndex,(!!), (\\),union, isPrefixOf,delete,deleteBy,lines, unionBy)
import Data.Maybe(fromJust, fromMaybe, catMaybes, isNothing)
import qualified Data.HashTable as H
import System.Directory
import System.IO
import System.IO.Unsafe

import Debug.Trace
import Control.Exception
import Control.Concurrent(forkIO)

import Lang (cgiURL,evertxt,nowtxt,timeLocale, timeFormat,yes,no,complaint,notMember,approbalquestion,doesnotexist,grnamenotavail)
import LangEs (evertxt,nowtxt,timeLocale, timeFormat,yes,no,complaint,notMember,approbalquestion)


import Data.TCache
import Data.TCache.DefaultPersistence
import qualified Data.ByteString.Lazy.Char8 as B
import ReadFile
import Data
import Search
--import WebObject



debug a b = trace b a

withVResource :: ResourceVote->(Maybe ResourceVote->ResourceVote)->IO ()
withVResource r f= withVResources [r] (\[mr]-> [f mr])

-- added indexation
-- TODO : to integrate the search actualization in TCache
withVResources:: [ResourceVote]->([Maybe ResourceVote]->[ResourceVote])->IO ()
withVResources as f=  withResources as f'

 where
     f' mas= (unsafePerformIO $ forkIO $ mapM_ filterIndex as') --index it in a different thread
                         `seq` as'
           where
                        as'= f mas

filterIndex :: ResourceVote -> IO ()
filterIndex a= case a of
        Ru x -> addObjectSearch (keyResource x) $ HsReg x
        Rp x -> addObjectSearch (keyResource x) $ HsReg x
        Rs x -> addObjectSearch (keyResource x) $ HsReg x
        Ra x -> addObjectSearch (keyResource x) $ HsReg x
        _ -> return ()


--withVResourcesID :: [ResourceVote]->([Maybe ResourceVote]->[Operation ResourceVote])->IO ()

withVResourcesID as f=  withResources as f -- f'
{-
 where
     f' mas= (unsafePerformIO $ forkIO $ mapM_ filterIndexRes as') --index it in a different thread
                         `seq` as'                              --return the result
           where
                as'= f mas
                filterIndexRes a= case a of
                        Insert x -> filterIndex x
                        Delete x -> unfilterIndex x
                                
-}
unfilterIndex x= case x of
        Ru x -> addObjectSearch (keyResource x) $ Words ""
        Rp x -> addObjectSearch (keyResource x) $ Words ""
        Rs x -> addObjectSearch (keyResource x) $ Words ""
        Ra x -> addObjectSearch (keyResource x) $ Words ""
        _ -> return ()

addVResources  l= withVResources [] (\_->l)
addVResource c= addVResources [c]
getVResource :: ResourceVote ->  IO (Maybe ResourceVote)
getVResource  = getResource 

getVResources:: [ResourceVote] ->  IO [Maybe ResourceVote]
getVResources =getResources

-- return error if any resource is not found
justGetVResources rs=do mrs <- getVResources rs
                        return $ map process $ zip mrs rs  
        where
            process (Nothing, r) = error ("\""++keyResource r ++ "\" does not exist")
            process (Just r', _) = r'
    
justGetVResource r= do  [r']<- justGetVResources [r]
                        return r'

                 

deleteVResource :: ResourceVote -> IO ()
deleteVResource= deleteResource


syncVCache= syncCache
-- do  syncCache (refcache :: Cache ResourceVote) 
--                writeIndex indexFname --index of the search engine
                 
                

clearSyncVCacheProc= clearSyncCacheProc-- (refcache :: Cache ResourceVote)

data ResourceVote=    Ru User | Rp Project | Rs Subject  | Ra  Subject 
                    -- | Rf WebObject 
                    | Rl {lname:: String, lcontent :: [String]}
                    | Rt {tname:: String,crontab::[(Integer,CronOp,[String])]} 
        
                    deriving(Show,Read,Typeable)
 

instance Data.TCache.DefaultPersistence.Indexable ResourceVote where
        key   (Ru a)    = keyResource a
        key   (Rp a)    = keyResource a
        key   (Rs a)    = keyResource a
        key   (Ra  a)   = keyResourceAmend a
        key   (Rl n _)  = n
        --keyResource   (Rf wo)   = getName wo
        key   (Rt n _)  = n

instance Serializable ResourceVote where
    serialize= B.pack . show
    deserialize= read . B.unpack
 {-       
instance  IResource ResourceVote where
        keyResource   (Ru a)    = keyResource a
        keyResource   (Rp a)    = keyResource a
        keyResource   (Rs a)    = keyResource a
        keyResource   (Ra  a)   = keyResourceAmend a
        keyResource   (Rl n _)  = n
        --keyResource   (Rf wo)   = getName wo
        keyResource   (Rt n _)  = n


        
        readResource  (Ru a)    = do mb <- readResource a 
                                     case mb of  {Just b-> return $! Just( Ru b);_->return Nothing}
        readResource  (Rp a)    = do mb <- readResource a 
                                     case mb of  {Just b-> return $! Just( Rp b);_->return Nothing}
        readResource  (Rs a)    = do mb <- readResource a 
                                     case mb of  {Just b-> return $! Just( Rs b);_->return Nothing}
        readResource  (Ra  a)   = do mb <- getAmend a
                                     case mb of  {Just b-> return $! Just( Ra  b);_->return Nothing}
                                    
        readResource  r= handle (\(e :: IOErrors) -> return Nothing)$
                                          do    content <- fReadFile $! dataPath++keyResource r
                                                return $! Just  content

        
        writeResource (Ru a)    = writeResource a 
        writeResource (Rp a)    = writeResource a
        writeResource (Rs a)    = writeResource a 
        writeResource (Ra a)    = addAmend a
        writeResource r         = fWriteFile (dataPath++keyResource r) r
        
        
        delResource (Ru a) = delResource a
        delResource (Rp a) = delResource a
        delResource (Rs a) = delResource a
        delResource (Ra a) = deleteAmend a 
        delResource r      = removeFile (dataPath++keyResource r)  
-}
instance IResource User where
        keyResource     e= "User."++name e      
        readResource    e= ifGetUser (name e)                           
        writeResource   e= addUser e
        delResource     e= deleteUser $ name e

instance IResource Project where
        keyResource     e= "Project."++pname e  
        readResource    e= getProject (pname e)                 
        writeResource   e= addProject e        
        delResource  e= do        
                 namedel <- getNameDeleted (dataName++"#deleted") 0
                 renameDirectory dataName (dataName++"deleted")    
                 where
                 dataName= dataPath ++ pname e
                 getNameDeleted name n= do
                   ask<- doesDirectoryExist $! name++show n
                   case ask of
                      True  -> getNameDeleted name $ n+1
                      False -> return $! name++show n



instance IResource Subject where
        keyResource     e= "Subject."++prosname e++"/"++sname e 
        readResource    e= getSubject e
        writeResource   e= addSubject e
        delResource  e= removeDirectoryRecursive $ dirSubject e


dirSubject e = dataPath++(prosname e) ++ "/" ++(sname e)

keyResourceAmend        e= "Amend."++aname e++ sname e++prosname e



ifGetUser name=handle  (\(e ::IOError) -> return Nothing) $ do    
        u<- getUser name
        return $ Just $ u
  where
  getUser name= fReadFile (dirusers++name++".xml"):: IO User


dirusers= dataPath++"users/"

addUser u= handle  (\(e ::IOError) -> handleIt e) $
            fWriteFile (dirusers++name u++".xml")  u
            where
	    handleIt e=do createDirectory dirusers
                          fWriteFile (dirusers++name u++".xml")  u


deleteUser email= do
        removeFile $ dirusers ++email++".xml" 
 

getProject:: String -> IO (Maybe Project)
getProject name = do
        handle  (\(e ::IOError) ->return Nothing) $ 
                (do     p <- fReadFile (dataPath++name ++"/"++"project.xml")
                        return $ Just p) 

addProject p= handle  (\(e ::IOError) ->handleIt e) $
                fWriteFile (dirproject++"/"++"project.xml") p 
                where
		dirproject= dataPath++pname p
                handleIt e= do createDirectory dirproject
                               fWriteFile (dirproject++"/"++"project.xml") p 



getSubject s= handle  (\(e ::IOError) ->return Nothing)$ 
                             do s <- fReadFile (dirSubject s++"/"++"subject.xml")
                                return $ Just s
      


addSubject sub= handle  (\(e ::IOError) -> handleIt e) $ fWriteFile name sub 
        where 
	dir= dirSubject sub
        name    = dir++"/"++"subject"++".xml"
        handleIt e= do createDirectory dir
                       fWriteFile name sub


diramends amd= dirSubject  amd ++ "/amends/"
fileamend amd= diramends amd ++ aname amd

addAmend amd= handle  (\(e ::IOError) -> handleIt e) $ fWriteFile file  amd 
     where
     dir= diramends amd 
     file= dir++ aname amd
     handleIt e= do createDirectory dir
                    fWriteFile file  amd

deleteAmend amd= removeFile $ fileamend amd


getAmend amd= handle  (\(e ::IOError) ->return Nothing)$ do
                                    a <- fReadFile $! fileamend amd :: IO Subject
                                    return $! Just a 
                                    

-- key for the amends of a subject
listAmendsNamePs project subject= project++"/"++subject++"/amends/listAmends"
-- filename where the list of amends are located
listAmendsName s= diramends  s ++ "listAmends"

--fileAmend project subject name= dataPath++project++"/"++subject++"/amends/"++name

--amendDir s= prosname s++"/"++sname s++"/amends/"

--fileAmenda a= dataPath++amendDir a++aname a


secsDay= 24*60*60
pubProjects="publicProjects.xml"
visProjects="visibleProjects.xml"
config= "config.xml"
cronTab = "cronTab.xml"
indexFile= "index"

fReadFile path= do
        s <- readFile1 path
        return $ read s
        

fWriteFile:: (Show v)=> String-> v -> IO()
fWriteFile path v = writeFile path $ show v 


------------------------Search Auxiliary functions Search.hs---------------

indexFname=dataPath++"Index.idx"

        
instance Search.Indexable ResourceVote where
        listIze a= listIze $ HsReg a

idToText a= show a
expSeparators a="{},=,."

getFromURI uri= do
  obj<- getIt uri
  return (idToText$ obj,expSeparators obj)
 where
  getIt uri=
     case listIze $ Words uri of
        ["User",n]->     justGetVResource $ Ru uUser{name=n}
        ["Project",n]->  justGetVResource $ Rp uProject{pname=n}
        ["Subject",p,s]->justGetVResource $ Rs uSubject{prosname=p,sname=s}
        ["Amend",p,s,a]->justGetVResource $ Ra uSubject{prosname=p,sname=s,aname=a}
        _ -> error $ "URI unknown: "++ uri


-- from a set keys in a string, return for each document matched, the uri identifier
-- and a excerpt with the keywords searched for and the relevance for each paragraph

searchResults:: String->IO[(String,[(String,Int)])]
searchResults keys =do
  let xs=listIze $ Words keys
  uris<- search xs 
  rs  <- mapM getFromURI uris
  return $ zip uris (map (showExcerpts xs) rs)



        

-------------------------Lists,cronTab auxiliary Aprobal.hs----------------------------

--add an entry in a Rt (tasklist) resource
addEntry lname entry = withVResource (Rt lname undefined) $ addc entry 
    where 
        addc  entry (Just (Rt _ l))= Rt lname (entry:l)
        addc  entry Nothing= Rt lname  [entry]


-- delete an entry in a Rl resource     
delEntryBy f lname entry = withVResource (Rt lname undefined) $ delc entry 
    where
        delc _ Nothing= error ("list "++ lname++" does not exist")
        delc entry (Just(Rt n l))= Rt lname $  deleteBy f entry l


delCronTab  entry= delEntryBy fdel cronTab entry where
    fdel (_,fname,args)(_,fname',args')= fname==fname' && args==args' 
    
------------------------Users---------------------------------
                        


userValidate pname ppass = do
        jus <- getResource $! Ru uUser{name= pname}
        case jus of
                Nothing-> return False
                Just (Ru us)->
                        if  ppass == password us then return True 
                        else return False


userRegister pname pass= do
   handle  (\(e :: SomeException) ->return False)$ do
        withVResource ( Ru uUser{name=pname}) doit
        return True
   where
   doit Nothing = Ru blankUser{name=pname,password=pass,uindex=0}
   doit _   =    error ""
 


getUserByIndex:: Int->String-> IO User
getUserByIndex index project=do
        Just (Rp pr) <- getVResource $ Rp uProject{pname= project}
        Just (Ru us) <- getVResource $ Ru uUser{name= (users pr) !! index}
        return us

indexUser:: String->String-> IO Int
indexUser email project= do
        Rp pr <- justGetVResource $ Rp uProject{pname=project}
        return (fromJust $ findIndex (email==) $ users pr)

---------------------projects------------

projectListNames name= do
                jRu  <-  getVResource$ Ru uUser{name=name}
                case jRu of
                 Just (Ru us)-> return $ map uObject $ uProjects us
                 Nothing -> userNotExist

userNotExist= error (" Your user does not exist. <a href="++cgiURL++"?op=vor&email=&pass=&val= >Please log-in as a valid user</a>")

subjectListNames name= do
                jRu  <-  getVResource$ Ru uUser{name=name}
                case jRu of
                 Just (Ru us)-> return $ map uObject $ usubjects us
                 Nothing -> userNotExist


projectList user= do
                prList  <-  projectListNames user
                prs     <-  getProjects $ prList
                return $ catMaybes prs

subjectList user=do
        subList<- subjectListNames user
        getSubjects subList

getSubjects names=do    
        jsubs<-getVResources [Rs uSubject{sname=tail sub,prosname=pr}| (pr,sub) <- map splitSubject names]
        return $! map (\(Rs s)-> s ) $ catMaybes jsubs
        where
        splitSubject name= splitAt (justFindIndex (=='/') name)  name       
        justFindIndex f xs= case findIndex f xs of
              Just n  -> n
              Nothing -> error $ "delegated proposal:"++xs++" has not project/subject name"


getProjects names=do    jrs<-getVResources [Rp uProject{pname=name}| name <- names]
                        return $! map (\(Rp p)-> Just p )$ catMaybes jrs
                        

projectRegister p= 
   handle  (\(e ::ErrorCall) -> handleIt e) $ do
    withVResources  [Rp uProject{pname=pname p},uRl {lname=pubProjects},uRl {lname=visProjects}] process
    return True
  where
    handleIt e=do
    	logEntry $ "Warning: " ++ show e
    	return False

    name= pname p
    
    process[Nothing,Just pubs,Just vis]= [Rp p, if public p  then pubs{lcontent=name:lcontent pubs}else pubs,
                                                  if visible p  then vis{lcontent=name:lcontent vis}else vis]

    process r =  error $ "projectRegister: "++name++" withVResources returned: "++show r 



deleteProject pname = 
   do
	Rp pr<- getResource (Rp uPr) `onNothing` error ("deleteProject "++ pname)
	atomically $ withSTMResources
            (uRl{lname=visProjects}
            :uRl{lname=pubProjects}
            :[Ru uUser{name=n}| n <- users pr])
            doit 
	return ()

        where
        uPr= uProject{pname=pname}
        doit  (Just (Rl vn vis ):Just (Rl pn pub ):usrs)= resources{
             toDelete= [Rp uPr]
             ,toAdd=     Rl vn (Data.List.delete pname vis)
                        :Rl pn (Data.List.delete pname pub)
                        :map removeDir (catMaybes usrs)}

        doit  r = error $ "on deleteProject, "++ pname ++ ":withVResourcesID returned "++ show r

        removeDir  (Ru user)= Ru user{uProjects= myDeleteBy (\up->uObject up==pname) $ uProjects user}
        myDeleteBy f xs=filter (\x->not $ f x) xs       

publicProjectNames= do
    mlistt<- getVResource uRl {lname=pubProjects}
    case mlistt of
       Nothing -> return []
       Just (Rl _ pubprnames) -> return pubprnames
    
publicProjects = do
    pubprnames <- publicProjectNames
    jress<- getVResources[Rp uProject{pname=n}| n <- pubprnames] 
    return $! map (\(Just (Rp pr))->pr)  jress

----------------------------Subject------------------------
question lang s = case (options s) of
                        Approbal        -> mapprobalquestion
                        ChooseOptions q _ _ -> q
                        
    where mapprobalquestion= case lang  of
            "es" -> LangEs.approbalquestion
            _    -> Lang.approbalquestion
    
stringOptions lang s= case (options s) of
        Approbal                -> apoptions 
        ChooseOptions _ _ ops   -> map (\(Option s _)->s)ops
    where
        apoptions=case lang of
                        "es" -> [LangEs.yes,LangEs.no,LangEs.complaint]
                        _    -> [Lang.yes,Lang.no,Lang.complaint]




getAmends project subject=  do
        mblist <- getResource uRl{lname=listAmendsNamePs project subject} 
        case mblist of
            Just (Rl _ amends) -> return amends
            Nothing  -> return []
        


diffAmend s amd= 
        case (content amd) of
                Str cont -> amd{content= Changes  $ diff (contents s) cont}
                _        -> amd             

 where  
        contents s= str where Str str=content s
 
        

addOne x xs=
        case (elem x xs) of
                True    -> xs
                False   -> x:xs

deleteSubject pname sname= atomically $
        withSTMResources [Rp uProject{pname=pname},Rs uSubject{sname=sname}] doit
        where
        doit [Just (Rp pr),Just (Rs sub)]=resources
           {toAdd= [Rp pr{psubjects= Data.List.delete sname (psubjects pr)}]
           ,toDelete= [Rs sub]}
	doit _ = error $ "file does not exist in deleteSubject project=" 
                          ++ pname ++ "Subject=" ++ sname 
        --TODO: WARNING need to erase the user entries too.

{-- create the votes array  for the subject
addSubjectVotes pr  s  
            | sstatus s /= Processing = s 
            | otherwise=   s{votes= voteSlots pr s }
-}
          

-- add spare vote slots to the votes array
voteSlots pr s= 
        let lvotes= l+1 where (0,l)= bounds v
            v = votes s
            lusers= length $ users pr  ----TODO avoid length 
            delta=  lvotes - lusers
            buffer= 20 -- spare slots more
            spare= take buffer $ zip [lvotes, lvotes+1..] (repeat (noVoted,IndexVote 0))
        in  if delta<=1 then v else array(0, lusers-1) $ assocs v ++ spare
        

voteSlotsSum  s=
      let lvotes= l+1 where (0,l)= bounds v
          v= sumVotes s
          lusers= case options s of
                     Approbal -> 4
                     ChooseOptions _ n _ ->   n+2
          delta= lusers - lvotes
          spare= take delta $ zip [lvotes, lvotes+1..] (repeat  0)
      in  if delta==0 then v else array(0,lusers-1) $ assocs v ++ spare


-----------------------users- Projects------------

-- TODO: check that the users are not registered, then send a mail requesting
 
addUserToProject us pr= withVResources [Ru us,Rp pr] 
                          (\[Just(Ru us'),Just(Rp pr')]->
                            [Ru us'{uProjects=ObjectUser project [] "":uProjects us}
                            ,Rp pr'{users= users pr++[email]}])
         where project=pname pr
               email= name us               

processNames :: String-> [String] -> IO ()

processNames p nms  =
  do 
        
        mapM (processName p) nms
        return ()  
  where
        processName p name =withVResource (Ru uUser{name=name})proc
                        
          where
                proc  Nothing        = Ru $! sendMail name `seq` nuser blankUser
                proc (Just (Ru us))= Ru $! nuser us

                nuser user= blankUser{name=name,uProjects =  unionBy (\a b->uObject a==uObject b) (uProjects user)[ObjectUser p [] ""]}

                sendMail u=  ()


---------------------parse--------------------------------------

subst [] _ _= []
subst (z:xs) x y | z==x = y:xs
                  | otherwise = x:subst xs x y  

replaceBy [] _ _ = []
replaceBy (x:xs) f a    | f x == True   = a:xs
                        | otherwise     = x:(replaceBy xs f a)


replace [] _ _ = []
replace xss@(t:xs) s s2 |isPrefixOf s xss       = s2++ replace (drop(length s)xss) s s2
                        |otherwise              = t:replace xs s s2


skipspaces [] _ = []
skipspaces (c:rest) f  | not(f c)       = skipspaces rest f
                       | otherwise      = c:rest


myreads [] _=([],[])
myreads string f= myreads1 (skipspaces string f) f


myreads1 [] _=([],[])
myreads1 (c:rest) f  | not(f c)  = ([],c:rest) 
                     | otherwise = (c:r, skipspaces s f) 
                                   where (r,s) = myreads1 rest f



str2List [] _   = []
str2List s  f   = h:r where     (h,t)   = myreads s f
                                r       = str2List t f
                                
-- drop heading and trailing whitespaces
unSpace str= reverse $! dropHead $! reverse $ dropHead str
 where
    dropHead s=dropWhile isSpace s
 

strToList   s   = map unSpace $! str2List s (\c ->isAlphaNum c || c=='@' || c=='.' || c==' ') 

--strLnToList s = str2List s (\c -> c /= '\x0d'$$ c/= '\x0a')



myreads2 [] _=([],[])
myreads2 (c:rest) f  | not(f c)  = ([],c:rest) 
                     | otherwise = (c:r, s) 
                                   where (r,s) = myreads2 rest f





diff2 rs xs []          = (rs, xs,[],[]) 
diff2 rs [] ys          = (rs++ys,[],[],[])
diff2 rs xs (y:ys)      = case( myreads2 xs (y/=) ) of
                                (_, []) ->  diff2 (rs ++ [y]) xs ys
                                (zs,ts) ->  (rs,zs,ts,y:ys)    --(parte de y a�adida, parte de x quitada, resto de x para seguir)



diff11 [] [] _= []
diff11 (x:xs) (y:ys) n  | x==y  = diff11 xs ys (n+1)

diff11  xs ys n   = case(diff2 [] xs ys) of
                        ([],[],[],[])   -> []
                        (rs,zs,[],[])   -> [Change n zs rs]
                        (rs, zs, xs', ys') -> Change n zs rs: diff11 xs' ys' (n+length rs) 


diff1 xs ys= diff11 xs ys 0 


diff a b= diff1  (lines1 a) (lines b)

lines1 ss = map deCR $ lines ss where
    deCR s  |head rs=='\r' = reverse $ tail rs
            |otherwise= s
     where
        rs= reverse s
                ------apply,show changes----- 

applyChanges xs []= xs
applyChanges xs (Change n delete add:rdiff)  = applyChanges (zs ++ apply delete add ts ) rdiff 
   where

        (zs,ts)=splitAt n xs
        apply delete add ts = assert (isPrefixOf delete ts) add++drop (length delete) ts


diff2Changes xs [] _ = [Normal xs]
diff2Changes xs (Change n delete add:rdiff) m  = Normal zs: Deleted delete: 
                        Added add:diff2Changes (apply delete add ts) rdiff (length zs+length add) 

   where
        (zs,ts)=splitAt (n-m) xs 
        apply delete add ts = drop (length delete) ts -- `verify` ( isPrefixOf delete ts) 

        
data ChangeT= Normal[String] | Deleted [String] | Added [String]


instance Show ChangeT where
        show (Normal x)= show x
        show (Deleted x)= "Deleted " ++show x
        show (Added x)= "Added" ++ show x



------------------------------------------Delegation management--------------------



searchOu  o ou= o==uObject ou
objectUser us object  typeObject=find (searchOu object) (typeObject us)
objectUserDelegate us object typeObject= case (objectUser us object typeObject) of
                        Nothing -> Nothing
                        Just ou -> case (delegatedTo ou) of
                                        ""  ->Nothing
                                        oud ->Just oud

objectUserDelegationList us object typeObject=
                          case (objectUser us object typeObject) of
                                Nothing -> Nothing
                                Just ou -> Just ( delegated ou)

addIfnotMember a b= case (elem a b) of{True-> b;False-> a:b}

usObject us typeo content= case typeo of
                                "utopics"       -> us{utopics   = content}
                                "usubjects"     -> us{usubjects = content}
                                "uProjects"     -> us{uProjects = content}
        


        --for each change of deletate is necessary to change the content of 3 users:
        --      remove the user index from the -former delegate- (sDelegate) list (replaceFormerXXXDelegate)
        --      add this index to the sDelegate list of the -new delegate- (replaceXXXDelegate)
        --      insert the delegate name to the sDelegatedTo field of -the user- topic/subject (replaceXXXTo)

changeDelegate :: String->String->String->(User -> [ObjectUser], String)->String->String->IO()
changeDelegate lang email object typeObject@(typeO,str) delegate project=do
        Just (Rp pr) <- getVResource$ Rp uProject{pname=project}
        case (delegate `elem` (users pr)) of
            False          -> withVResource  (Ru uUser{name=email}) (\(Just (Ru us))->
                                    Ru $! replaceObjectTo us object mnotMember typeObject)
            True  -> do
                index <- indexUser email project
                Just (Ru us) <- getVResource $ Ru uUser{name=email}

                case (objectUserDelegate us object typeO) of

                        Nothing ->  withVResources[Ru uUser{name=email},Ru uUser{name=delegate}]
                            (\[Just (Ru us),Just (Ru del)] -> 
                                [Ru $! replaceObjectTo us object delegate typeObject
                                ,Ru $! replaceObjectDelegate email object del typeObject index
                                ]

                            )
                        Just former-> 
                          
                          if former== delegate then return ()
                          else withVResources[Ru uUser{name=email},Ru uUser{name=delegate},Ru uUser{name=former}] doit
                            where 
                             doit[Just (Ru us),Just (Ru del),Just (Ru formeru)]= 
                                [Ru $! replaceObjectTo us object delegate typeObject 
                                ,Ru $! replaceObjectDelegate email object del typeObject index  
                                ,Ru $! replaceFormerObjectDelegate email formeru object typeObject index
                                ]
                             doit[Just (Ru us),Just (Ru del),Nothing]= 
                                [Ru $! replaceObjectTo us object delegate typeObject 
                                ,Ru $! replaceObjectDelegate email object del typeObject index
                                ]
                                

        
   where
        mnotMember= case lang of
            "es"  -> LangEs.notMember
            _     -> Lang.notMember
            
        replaceObjectTo:: User->String->String->(User -> [ObjectUser], String)->User
        replaceObjectTo us o d (typeObject,typeo)=
                case (objectUser us o typeObject) of 
                                Nothing -> usObject us typeo $ ObjectUser o [] d:uosUs
                                Just ou -> usObject us typeo $ replaceBy uosUs (searchOu o) ou{delegatedTo=d}

                where   
                        uosUs= typeObject us



        --add the user name to the list of delegated in the delegate�s list
        replaceObjectDelegate :: String->String->User->(User -> [ObjectUser], String)->Int->User
        replaceObjectDelegate user o del (typeObject,typeo) indexu=

                        case (objectUser del o typeObject) of 
                                Nothing -> usObject del typeo $ ObjectUser o [indexu] "":uosUs
                                Just ou -> usObject del typeo $ replaceBy uosUs (searchOu o) ou{delegated=addIfnotMember indexu (delegated ou)}

                                where   
                                        uosUs= typeObject del


        -- erase the user in the delegation list of the former delegate
        replaceFormerObjectDelegate email us o (typeObject,typeo) index=

                case (objectUser us object typeObject) of       
                        Just ou  -> usObject us typeo$ replaceBy (typeObject us) (searchOu object) ou{delegated= Data.List.delete index (delegated ou )}                  
                        Nothing  -> error $! "delegate:"++Data.name us++"has no data for object:"++o


 
---------------------------Vote-----------------------------------
-- rangeForVote return the number of days for the end of the votation

flastVote  daysb= 	tnow+daysb*secsDay

tnow= n where TOD n _ = unsafePerformIO getClockTime

rangeForVote s tnow=  
        if last==0 then 0 else fromIntegral( (last -tnow) `div` secsDay)
  where
                
                last = lastVote s

showTime lang t= formatCalendarTime  mtimeLocale rfc822DateFormat  ( toUTCTime tt) where 
    tt= TOD t 0 
    mtimeLocale= case lang of
            "es" -> LangEs.timeLocale
            _    -> Lang.timeLocale
            
subjectTimeParms:: String -> Subject-> (String, String)
subjectTimeParms lang s= (endTime,startTime) where

        endTime   = if last==0 then mevertxt else showTime lang  last
        startTime = if daysb==0 then mnowtxt else showTime lang $ last - (fromIntegral daysb)*secsDay
        last = lastVote s
        daysb= daysBefore s

    
        (mevertxt,mnowtxt, mtimeFormat)= case lang of
                "es"-> (LangEs.evertxt, LangEs.nowtxt, LangEs.timeFormat)
                _   -> (Lang.evertxt, Lang.nowtxt, Lang.timeFormat)


subjectRangeVote s = return $ rangeForVote s tnow


hasVotedUser email svotes project= do
        vote <- voted
        if vote /=(Priority 0, IndexVote 0) then return $ Just vote
         else return Nothing

 where
        voted    = do
                        index <-indexUser email project
                        if index > length  then return (Priority 0,IndexVote 0)
                                else return $ svotes !(index)
        length= u+1 where (0,u)= bounds svotes




hasSubjectUser subject us = find (\s -> (uObject s)== subject)(usubjects us)


hasDelegated subject us =  case (hasSubjectUser subject us)of
                                        Nothing         -> Nothing
                                        Just subjuser   -> Just (delegatedTo subjuser)



topicDelegatedTo us topic= case (find (\tu->uObject tu==topic) (utopics us))of
                                        Nothing   -> Nothing
                                        Just u    -> Just (delegatedTo u)



-- addVotes votes us iVote=   votes // map(\(p,i)-> (i, (p, iVote)))  us   




-- vote process. each user has tree  potential types of representation:  bysubject, bytopic, global in order of preference. 

repBy typeR us object = case (find(\ou-> uObject ou== object) (typeR us))of
                                        Nothing   -> []
                                        Just u    -> delegated u

repBySubject subject us=repBy usubjects us subject  

repByTopics topics us= concatMap (repBy utopics us) topics 

repByProject project us=  repBy uProjects us project



represent:: String ->Subject -> User-> [PriorIUser]
represent project sub us= 
                (map (\x -> ( bySubject, IndexUser x)) s ++
                 map (\x -> ( byTopic, IndexUser x)) d     ++
                 map (\x -> ( byProject,  IndexUser x)) e  ) 
                  

        where   s= repBySubject (sname sub) us
                t= repByTopics (topics sub) us
                g= repByProject project us


                d= t\\s
                e= g\\(union s t)



{-
once we have the complete list, each , for each list element, if the cathegory of the vote 
is greather than the existent, 
repeat the votation process with his respective represented people.
-}

representedVotes initialIndex iVote project (sub,lvotes) us= checkPriorities represented project (sub,lvotes) iVote where

        represented= represent project sub us `debug` "representedVotes"
   
        checkPriorities list project (sub,lvotes) iVote = do 
                l  <-  mapM (checkPriority iVote project (sub,lvotes)) list 
                return $  concat l

        checkPriority iVote project (sub,lvotes) v@(Priority priority, IndexUser index)= 
              if index == initialIndex then return []  --to avoid circular representations
               else do
                us <- getUserByIndex index project
                c  <- check  iVote lvotes (name us) v  project
                case c of
                        LT -> return []
                        _  -> do final<-representedVotes initialIndex iVote project (sub,lvotes)  us
                                 return (v:final)


                where   check iVote lvotes email (typev,IndexUser index) project=do
                                vote  <- hasVotedUser email lvotes project
                                case vote of
                                        Nothing -> return GT

                                        Just (type1, _)-> return $ compare typev  type1 


uncategorize l= map splitIndex l
        where splitIndex (_, x)= x

voteProc:: Int->Subject->
            Project->Int->[PriorIUser]->(Array Int PriorIVote, Array Int Int)

voteProc ivote sub   pr  indexu votes=(lvotes,lsumVotes) where
      lvotes= nvotes // map(\(Priority p,IndexUser i)-> (i, (Priority p, IndexVote ivote)))  votelist
      lsumVotes= accum (+) (sumVotes  sub)  $ res `debug` show res  
                 
                    where
                       res= concatMap  swap votelist
                       swap :: PriorIUser -> [(Int,Int)]
                       swap (Priority p, IndexUser i) 
                           | p >=p'    =  [(v ,1),(v',-1)]
                           | otherwise =  [(v',1),(v ,-1)]
                          where
                                 (Priority p', IndexVote v')= nvotes ! i
                                 v=ivote :: Int
 
      nvotes= voteSlots pr sub
      votelist= (direct, IndexUser indexu):votes
      
         
iVote vote options= case( findIndex (vote == ) options ) of
                Nothing -> 0 
                Just i  -> i+1 

-----------------------Initialization-----------------------------
dataPath="data/"

checkRepository=do
    checkDir dataPath
    logEntry $! "Using data at= "++ dataPath
    
    checkDir  usersDir 
    checkList pubProjects 
    checkList visProjects 
    syncVCache
    
 where
    checkList listFile=do
        exist<- doesFileExist $! dataPath++listFile
        case exist of
            False -> withVResources [] (\_-> [Rl listFile []])
            True  -> return()
 
    checkDir dir= do
        exist <-doesDirectoryExist dir
        case exist of
            False -> createDirectory dir
            True  -> return ()
            
    usersDir=dataPath++"users"
            

blankProject = Project "" "" "" [] [] [] False False 
blankUser    = User "" "" 0 [] [] [] 
blankSubject = Subject "" "" "" "Ordinary" [] [] Draft (Str "")  Approbal
               (read "array (0,0)[(0,(0, 0))]")
               (read "array (0,0)[(0,0)]") 0 1 

-- for resource queries
uProject    = Project u u u u u u u u   where u = undefined 
uUser       = User u u u u u u  where u= undefined 
uSubject    = Subject u u u u u u u u u u u u u  where u= undefined 

uRl         = Rl undefined undefined
uRt         = Rt undefined undefined


-----------------------logging----------------------------
logHandle= unsafePerformIO $ openFile ( dataPath++ "log.log") AppendMode

logEntry str=do t   <- getClockTime 
                ct  <- return $ toUTCTime t
                putStrLn $ output ct
                hPutStrLn logHandle $ output ct
    where
                output ct = calendarTimeToString ct++": "++ str++"\n"

-----------------------------------------------------------




