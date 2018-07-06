{-# OPTIONS -fglasgow-exts #-}
module Search where
{-------------------------------------------------
-- The Core of a Search Engine
-- Author: Alberto Gómez Corona Nov 2006
-- Language: Haskell  Version 0.3
	- renamed addObject to addObjectSearch
	- added showExcerpts
	- handle  concurrent issues

-- Terms of use: you can do whatever you want
-- with this code as long as you keep this notice
------------------------------------------------}
 
import Data.HashTable as HT
import Control.Concurrent.MVar
import Data.Bits
import System.IO.Unsafe
import Data.Array
import Data.Maybe(catMaybes,fromJust)
import Data.List(isPrefixOf)
import Control.Exception
import ReadFile


import Data.Char(isAlphaNum)


-- to test the search

data Register=Reg String Integer deriving Show

{-
main=do

  addObjectSearch "input1.txt" ( Words "John, 18") 
  
  addObjectSearch "input2.html" ( XMLText "<html><body><p>John</p><body>18</body></html>") 
  
  addObjectSearch "Register" (HsReg $ Reg "John" 17) 

  r<- search ["John", "18"] indexes
  print r

-}


--a is indexable if it can be serialized in a list of word strings        

class Indexable a where
    listIze   ::a -> [String]


-- filter for plain text
data Words= Words String

instance Indexable Words where
  listIze (Words s)= str2List s (\c ->isAlphaNum c || c=='@') where 

   str2List [] _	= []
   str2List s  f	= h:r where 	
    (h,t)	= myreads2 s f
    r	= str2List t f

    myreads2 [] _=([],[])
    myreads2 (c:rest) f | not(f c)  = ([], rest) 
                        | otherwise = (c:r, skipspaces s f) 
		            where (r,s) = myreads2 rest f

    skipspaces [] _ = []
    skipspaces (c:rest) f  | not(f c)	= skipspaces rest f
     		       | otherwise	= c:rest

--filter for XML/HTML
data XMLText= XMLText String

instance Indexable XMLText where
  listIze (XMLText s)= str2List s (\c ->isAlphaNum c || c=='@') where 

   str2List "" _	= []
   str2List ('<':rs) f  = str2List (tail $ dropWhile(/='>') rs) f
   str2List s  f	= h:r where
    (h,t)	= myreads2 s f
    r	        = str2List t f

    myreads2 [] _=([],[])
    myreads2 (c:rest) f | not(f c)  = ([], rest) 
                       | otherwise = (c:r, skipspaces s f) 
			           where (r,s) = myreads2 rest f

    skipspaces [] _ = []
    skipspaces ('<':rs) f = skipspaces (tail $ dropWhile(/='>') rs) f
    skipspaces (c:rest) f  
       	| not(f c)	= skipspaces rest f
     	| otherwise	= c:rest

-- filter for haskell registers
data HsReg a= HsReg a
instance Show a => Indexable (HsReg a) where
    listIze  a = listIze $ Words $ show a

instance Show a=> Show(HsReg a) where
    show (HsReg a)= show a

-------------------------SEARCH----------------------------------------
--store the URI list of indexed objects (reverse to intKey)
keyInt= unsafePerformIO $ new (==) hashString :: HashTable String Int

--store the bitmaps for each word
searchIndex= unsafePerformIO $ new (==) hashString :: HashTable String Integer

--store the URN for each bit position
intKey= unsafePerformIO $ new (==) hashInt:: HashTable Int String

-- the last document number+1
lastIndex :: MVar Int
lastIndex= unsafePerformIO $ newMVar 0

readIndex filename=
  handleJust ioErrors(\e-> return ())$ do	
	s<-  readFile'  filename 
	let (index,keys,last)=  read s :: ([(String,Integer)],[(String,Int)],Int)
	mapM_ (\(w,bitm)->HT.update searchIndex w bitm)  index 
	mapM_ (\(uri,i)-> HT.update keyInt uri i) keys
	mapM_ (\(n,v)->   HT.update intKey v n) keys
	swapMVar lastIndex last
	return ()
	

writeIndex filename = do 
	htl<- HT.toList searchIndex
	ki<- HT.toList keyInt
	last <- readMVar lastIndex
	writeFile filename $ show (htl,ki,last)	

-- add the object content to the index, identified by the uri string:

addObjectSearch:: (Indexable a)=> String->a->IO()
addObjectSearch uri object= 
           addListSearch uri (listIze object) 

addListSearch uri list =do
  n<- do mk<- HT.lookup keyInt uri 
         case mk of
            Just k -> return k -- re-index of a known document
    	    Nothing-> do 
			 n<-takeMVar lastIndex 
			 putMVar lastIndex (n+1)
			 
			 return n	
 
  add n (k n) list
  update keyInt uri n
  update intKey n uri

  return ()

  where
   add n k  []= return ()
   add n k (x:xs)= do
	mv<- HT.lookup searchIndex x
	case mv of
	  	Just v -> update searchIndex x (v .|. k)
	  	Nothing-> update searchIndex x k
	add n k xs
    
   k n= shiftL 1 n 

-- search for a string of keywords. Return the list of URIs That contain all of them 
search :: [String]-> IO [String]
search xs
 |xs==[]= return [] 
 | otherwise= do
    n <- readMVar lastIndex 
  
    mbi<- mapM (HT.lookup searchIndex)xs 
    let bi = catMaybes mbi
    -- if a word has not been found then return empty results
    if length bi/= length mbi then return [] else do 
     let r= foldr (.&.) (head bi)  bi -- select te doc bits that have all the words  
         --r has the bits of the objects that match
  
     getObjecURIs n 0 r []
  
    where
      getObjecURIs :: Int->Int->Integer->[String]->IO [String]
      getObjecURIs 0 _ _ rs= return rs
      getObjecURIs n i r rs=
  	case (testBit r 0) of
  	  True -> do
  		  urn <- HT.lookup intKey i
  		  getObjecURIs (n-1)(i+1) (shiftR r 1) (fromJust urn:rs)
  
  	  False-> getObjecURIs (n-1)(i+1) (shiftR r 1) rs


-- return,for a list of keys "qs", a list of (paragraph,number-of-keys found) in a "text", 
-- with a separators list "sep" 

showExcerpts::  [String]-> (String,[Char])-> [(String,Int)]
showExcerpts qs (text,sep)=  se qs text [] 0 [] where 
	se :: [String]->String->String->Int->[(String,Int)]->[(String,Int)]
	se _ [] _ _ fr= fr

   	se qs t@(x:xs) f hasKeys fr
	  | (not.null) $ filter (==True)$ map ((flip  isPrefixOf) t) qs=  --some word found
 		se qs xs (append f  x) (hasKeys+1) fr

		
	  | x `elem` sep=  -- limit of fragment
		case hasKeys of
		 0 -> se qs xs [] 0 fr               --the previous fragment was void of keys
		 _ -> se qs xs [] 0 ((f,hasKeys):fr) --added the old, new fragment starts

	  | otherwise= se qs xs (append f x) hasKeys fr


	append l c= l ++ [c]
