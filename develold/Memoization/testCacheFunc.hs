import Data.TCache.Dynamic
import Control.Concurrent(threadDelay)
import CachedFunc
import Debug.Trace
import Data.TCache.IDynamic
import System.Mem.StableName
import System.IO.Unsafe
import Data.Time


----------------------example ---------------------------------


fact :: Integer -> Integer
fact 1= 1                      `debug` "calculating factorial"
fact x= x* fact (x - 1)

factIO :: Integer -> IO Integer
factIO x= return $ fact x
cachedFact = cached  factIO  

debug a b= trace b a


main1= do
    initCachedTypes
    t<- tcached 5  time 0
    print t
    threadDelay 1000000
    main

    where
    TOD time _= getCurrentTime

       
main= do
   initCachedTypes
   
   let x = 20 :: Integer

   cachedFact  20  >>= print
   cachedFact 20  >>= print
   syncCache
   

   es<- mapM cachedFact $ take 10 $ repeat  20
   
   print  es



