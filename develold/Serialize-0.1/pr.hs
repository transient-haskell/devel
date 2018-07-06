module Main where
import Data.RefSerialize
import System.Mem.StableName
import System.IO.Unsafe

--simple data structure
data S= S Int Int deriving ( Show, Eq)         


instance  RefSerialize S  where
    gshowParser (S x y)= do
                    xs <- gshowParser x
                    ys <- gshowParser y
                    return $ "S "++xs++" "++ys
       
     
    greadParser =  do
                    char 'S'
                    x <- greadParser
                    y <- greadParser
                    return $ S x y

----------------- a more complex structure with mixed record and array with default read/show type serialization ----

data Data = Data Int String deriving (Read,Show)

data Stat a= Workflows [String]
           | Stat{ wfName :: String, state:: Int, index :: Int, recover:: Bool, sync :: Bool , resource :: [a]} 
           | I a
           deriving (Read,Show)  

-- the parser definitions for this structure

instance RefSerialize a =>  RefSerialize (Stat a) where
    gshowParser (Workflows list)= do  str <- gshowParser list
                                      return $ "StatWorkflows "++ str
    gshowParser (I x) = return $ "I " ++ rShow  x 
    gshowParser  (Stat wfName state index recover sync resource)= do
       parsea <- gshowParser resource -- uses the array instance plus the default read/show predefined instance for any type
       return $ "Stat "++ show wfName ++" "++ show state++" "++show index++" "++show recover++" "++ show sync ++ parsea  
       
    greadParser = choice [rStat, rData, rWorkflows] where --choice between the three 
        rStat= do
              symbol "Stat" 
              wfName <- stringLiteral
              state <- integer
              index <- integer
              recover <- bool
              sync <- bool
              resource <- greadParser
              return $ Stat wfName (fromIntegral state) (fromIntegral index) recover sync resource 
              
        rData= do
               symbol "I"
               a <- greadParser
               return $ I a 

        rWorkflows= do
               symbol "StatWorkflows"
               list <- greadParser
               return $ Workflows list  

          
main=  do

   putStrLn "serialize String's"
   let x= "hello"
   let str= rShow x
   putStrLn $ "rShow "++ str++"= "++str
  
   let y= rRead str :: String
   print y
   print $ x==y
   
   putStrLn "serialize [a] "
   let x= 1 :: Int
   let xs= take 5 $ repeat x
   putStr  "show xs= "
   print xs
   putStr "rShow xs="
   let str= rShow  xs 
   putStrLn str
   let ys= rRead str :: [Int]
   print $ xs==ys
   
   putStrLn "a RefSerialize instance of Stat (see definition in this file)" 
   let stat0 = Stat{ wfName="", state=0, index=0, recover=False, resource=[], sync= True} 

   let data0= Data 0 ""

   let str = rShow  stat0{resource= (take 2 $ repeat  data0) ++ (take 2 $ repeat (Data 1 "1")) }
   putStrLn "references to the same address are identified by gshowParser. they point to the same variable in the serialized data"
   putStrLn $ "rShow "++ show data0 ++"= "++ str
   let stat1= rRead str :: Stat Data
   
   putStrLn "data that point to the same variable when serializeds point to the same memory address when deserialized"
   
   let addr x= (hashStableName . unsafePerformIO . makeStableName) x
   let x= (resource stat1 !! 0)
   putStr "first element of the resource list= " 
   print x
   putStr "address of this element= "
   print $ addr x
   let y= (resource stat1 !! 1)
   putStr "second element of the resource list= "
   print y
   putStr "address of this element= "
   print $ addr y
   print $ addr y== addr x
     
   print ".........other examples................"
   let r = runR greadParser "[\"juan\", \"pepe\"]" :: [String]
   print r
   let r=runR bool "True"
   print r
   let str= rShow  "hola"
   print str
   print $ (rRead str :: String)
   let a = 1 :: Int
   let b = 2 :: Int
   let str= rShow  [a,a,a,b,b,b]
   print str
   let xs= (rRead str :: [Int])
   print xs
   print  $  rShow  xs 


   let a= 5
   let s= S a a
   let str= rShow  s
   print str  
   let s'= rRead str 
   print s
   print $ s== s'
   print $ rShow  s'

   let c=5
       d=5
   print $ rShow  (S c d)
  
   




