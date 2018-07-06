module Main where

import System.Plugins

main= loadExec "Test.o" "main" 

loadExec:: String-> String->IO String          
loadExec file method = do    
    
               mv <- load file ["."] [] method
               case mv of
                LoadSuccess mod v ->   v :: IO String
                LoadFailure msg ->     return "error"
      
