{-# OPTIONS  -fglasgow-exts  #-}

module Main where
-------------------------------------------------
-- A example of Transactional cache usage (TCache.hs)
-- (Something like the Java Hibernate)
-- Author: Alberto Gómez Corona Nov 2006
-- Language: Haskell
-- Terms of use: you can do whatever you want
-- with this code as long as you keep this notice
------------------------------------------------

import Data.TCache.Dynamic
import Data.Typeable
import Control.Concurrent
import Control.Concurrent.STM(atomically)
import Debug.Trace
import System.IO.Unsafe
debug a b= trace b a

--1 and 4: The data elements to be used in the example: A user will repeatedly buy Items.

data  Data=   User{uname::String, uid::String, spent:: Int} |
              Item{iname::String, iid::String, price::Int, stock::Int}
 
              deriving (Read, Show,Typeable)

--3 The mappings between the cache and the phisical storage are defined by the interface IResource

--      to extract the resource unique key,

--      to read the resource from the physical storage,

--      to store it and

--      to delete the resource from the physical storage.

 

instance IResource Data where
        keyResource         User{uid=id}= id
        keyResource         Item{iid=id}= id        
        serialize x= show x
        deserialize x= read x
        defPath _ = "data/"  -- directory where the data is stored.
        
        -- other definable methods: readResource, writeResource delResource. here the default persistence in files are used
        
-- buy is the operation to be performed in the example

--4 withResources gets a partial definition of each resource necessary for extracting the key, 
--fill all the rest of the data structures (if found ) and return a list of Maybe Data. 
--BuyIt is part of the domain problem. it receive this list and generates a new list of 
--data objects that are updated in the cache. buyIt is executed atomically.


user `buy` item= do 
  atomically $
    withSTMResources[user,item] buyIt
  
 where

    buyIt[Just us,Just it]
       | stock it > 0= unsafePerformIO (threadDelay 2000000) `seq`
                       Res[Just us',Just it'] [] [] () `debug` "john buy a PC"
       | otherwise   = error "stock is empty for this product"
       
      where
       us'= us{spent=spent us + price it}
       it'= it{stock= stock it-1}


    buyIt _ = error "either the user or the item does not exist"


main= do
        registerType :: IO Data
        -- create resources (acces no resources and return two new Data objects defined in items)
        withResources[]items

        --11 PCs are charged  to the John´s account in paralel, to show transactionality
        --because there are only 10 PCs in stock, the last thread must return an error

        for 11 $ forkIO $ User{uid="U12345"} `buy` Item{iid="I54321"}
        
        --wait 5 seconds        
        threadDelay 50000000

        [us,it] <-  getResources [User{uid="U12345"}, Item{iid="I54321"}]
        
        print $  "user data=" ++ show us
        print $  "item data=" ++ show it
        
        -- write the cache content in a persistent store (invoque writeResource for each resource)
        -- in a real application clearSyncCacheProc can be used instead to adjust size and write the cache periodically

        syncCache --(refcache :: Cache Data)

        -- the files have been created. the files U12345 and I54321 must contain the result of the 11 iterations

  where
        items _=
              [User "John" "U12345" 0
              ,Item "PC" "I54321" 6000 10]
              
        for 0 _ = return ()      
        for n f= f >> for (n-1) f
