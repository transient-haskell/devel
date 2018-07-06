module Main where
import Data
import Vote
import FreeChooser
import System.Directory
import VCache
import Data.TCache.Dynamic(syncCache, refcache, Cache) 




main= do
  --crear proyecto
  let project="project11111111111111" 
  let email="pepe"
  let pass="3025" 
  removeDirectoryRecursive dataPath
  createDirectory "data"
  copyFile "DefaultConstitution.hs" "data/DefaultConstitution.hs"
  
  print "creating user"
  
  cgi1 [("op","vor"),("email", email),("pass", pass),("pass2", pass),("reg",""),("SCRIPT_NAME","/dist/build/-tmp")]


  print "creating project"
  const <- readFile "DefaultConstitution.hs"
  cgi1 [("op","cre"),("oldname",""),("type","create"),("name",project),("pdescrip","desc")
       ,("topics","t1,t2"),("users",""),("subjects",""),("ispublic","OFF"),("isvisible","OFF")
       ,("OK","OK"),("email","pepe")
       ,("propTypesStr",const)
       ]
  syncCache  `debug` "sync"
  Rp pr <- justGetVResource $ Rp uProject{pname=project}
  print "proyecto creado"
  print pr


