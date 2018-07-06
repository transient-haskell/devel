module VCache where
import Data.TCache.Dynamic

import Data

dataPath="data/"

withVResource :: ResourceVote->(Maybe ResourceVote->ResourceVote)->IO ()
withVResource =  withResource
                      
withVResources:: [ResourceVote]->([Maybe ResourceVote]->[ResourceVote])->IO ()
withVResources= withResources

{-
-- added indexation
-- TODO : to integrate the search actualization in TCache

withVResources as f=  withResources as f'

 where
     f' mas= (unsafePerformIO $ forkIO $ mapM_ filterIndex as') --index it in a different thread
                         `seq` as'
           where
                        as'= f mas

filterIndex :: ResourceVote -> IO ()
filterIndex a= case a of
        e@(Ru x) -> addObjectSearch (keyResource e) $ HsReg x
        e@(Rp x) -> addObjectSearch (keyResource e) $ HsReg x
        e@(Rs x) -> addObjectSearch (keyResource e) $ HsReg x
        e@(Ra x) -> addObjectSearch (keyResource e) $ HsReg x
        _ -> return ()
-}

withVResourcesID :: [ResourceVote]->([Maybe ResourceVote]->[Operation ResourceVote])->IO ()
withVResourcesID= withResourcesID
{-
withVResourcesID as f=  withResourcesID as f'

 where
     f' mas= (unsafePerformIO $ forkIO $ mapM_ filterIndexRes as') --index it in a different thread
                         `seq` as'                              --return the result
           where
                as'= f mas
                filterIndexRes a= case a of
                        Insert x -> filterIndex x
                        Delete x -> unfilterIndex x
                                

unfilterIndex x= case x of
        e@(Ru x) -> addObjectSearch (keyResource e) $ Words ""
        e@(Rp x) -> addObjectSearch (keyResource e) $ Words ""
        e@(Rs x) -> addObjectSearch (keyResource e) $ Words ""
        e@(Ra x) -> addObjectSearch (keyResource e) $ Words ""
        _ -> return ()
-}
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

clearSyncVCacheProc= clearSyncCacheProc 


     
instance  IResource ResourceVote where
        keyResource   (Ru a)    = "User."++name a
        keyResource   (Rp a)    = "Project."++pname a
        keyResource   (Rs a)    = "Subject."++prosname a++"/"++sname a
        keyResource   (Ra  a)   = keyResourceAmend a
        keyResource   (Rl n _)  = n
        --keyResource   (Rf wo)   = getName wo
        --keyResource   (Rt n _)  = n
        keyResource   (Rc _)    = "Conf#"
        serialize = show
        deserialize s= read s `debug1` ("read: "++ s)  
        defPath _= dataPath


  
keyResourceAmend        e= "Amend."++aname e++ sname e++prosname e
{-        
        readResource  (Ru a)    = do mb <- readResource a 
                                     case mb of  {Just b-> return $! Just( Ru b);_->return Nothing}
        readResource  (Rp a)    = do mb <- readResource a 
                                     case mb of  {Just b-> return $! Just( Rp b);_->return Nothing}
        readResource  (Rs a)    = do mb <- readResource a 
                                     case mb of  {Just b-> return $! Just( Rs b);_->return Nothing}
        readResource  (Ra  a)   = do mb <- getAmend a
                                     case mb of  {Just b-> return $! Just( Ra  b);_->return Nothing}
                                    

        
        readResource  r= handleJust ioErrors(\e->return Nothing)$
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








--dirSubject e = dataPath++(prosname e) ++ "/" ++(sname e)





ifGetUser name=handleJust ioErrors (\e -> return Nothing) $ do    
        u<- getUser name
        return $ Just $ u
  where
  getUser name= fReadFile (dirusers++name++".xml"):: IO User


--dirusers= dataPath++"users/"

addUser u= handleJust ioErrors handle $
            fWriteFile (dirusers++name u++".xml")  u
            where
	    handle e=do createDirectory dirusers
                        fWriteFile (dirusers++name u++".xml")  u


deleteUser email= do
        removeFile $ dirusers ++email++".xml" 
 

getProject:: String -> IO (Maybe Project)
getProject name = do
        handleJust ioErrors  (\e->return Nothing) $ 
                (do     p <- fReadFile (dataPath++name ++"/"++"project.xml")
                        return $ Just p) 

addProject p= handleJust ioErrors handle $
                fWriteFile (dirproject++"/"++"project.xml") p 
                where
		dirproject= dataPath++pname p
                handle e= do createDirectory dirproject
                             fWriteFile (dirproject++"/"++"project.xml") p 



getSubject s= handleJust ioErrors (\e->return Nothing)$ 
                             do s <- fReadFile (dirSubject s++"/"++"subject.xml")
                                return $ Just s
      


addSubject sub= handleJust ioErrors handle $ fWriteFile name sub 
        where 
	dir= dirSubject sub
        name    = dir++"/"++"subject"++".xml"
        handle e= do createDirectory dir
                     fWriteFile name sub


diramends amd= dirSubject  amd ++ "/amends/"
fileamend amd= diramends amd ++ aname amd

addAmend amd= handleJust ioErrors handle $ fWriteFile file  amd 
     where
     dir= diramends amd 
     file= dir++ aname amd
     handle e= do createDirectory dir
                  fWriteFile file  amd

deleteAmend amd= removeFile $ fileamend amd


getAmend amd= handleJust ioErrors (\e->return Nothing)$ do
                                    a <- fReadFile $! fileamend amd :: IO Subject
                                    return $! Just a 
                                    

-- key for the amends of a subject

-- filename where the list of amends are located


--fileAmend project subject name= dataPath++project++"/"++subject++"/amends/"++name

--amendDir s= prosname s++"/"++sname s++"/amends/"

--fileAmenda a= dataPath++amendDir a++aname a

-}
