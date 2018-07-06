import Control.Concurrent.STM
import Debug.Trace
import Data.HashTable as HTable

debug a b= trace b a

strict []= []
strict (r:rs)= r `seq` (r:strict rs)

strictIO :: IO [a]-> IO[a]
strictIO iox= do
    x <- iox
    return $ strict x
    
main = do
  mv <- takeBlocks [1,2,3]
  vx <- mapM (atomically . readTVar) mv
  r <-  atomically $
          do
            let v2= map (+3) vx
            mapM (uncurry writeTVar) $ zip  mv v2
            mapM readTVar mv  
  print r
  



--takeBlocks rs =   mapM ionewtvar  rs  

--ionewtvar x= atomically $ newTVar x `debug` "newTVar"	
takeBlocks rs= mapM checkBlock rs
checkBlock  r =do
	c <-  HTable.lookup cache keyr  `debug1` ("checkBlock: "++ show (keyResource r))
	case c of
		Nothing   -> do
                  mr <- readResource r `debug1` ("read "++keyr++ " hash= "++ (show $ hashString  keyr))
		  case mr of
                    Nothing -> return Nothing  `debug1` "Nothing"
	            Just r2 -> do 
                        tvr <- atomically $! newTVar r2  `debug1` "Just"
			case addToHash of
                           False -> return $ Just tvr
                           True  -> do 
			            update cache keyr (tvr, ti, 0) -- accesed, not modified
                                    return $ Just tvr
                           

		Just(tvr,_,_)  -> return $ Just tvr `debug1` "in cache"

	where 	keyr= keyResource r
		ti= t where TOD t _=unsafePerformIO getClockTime


cache =unsafePerformIO   HTable.new (==) hashString
				



