module VParameters where

import System.IO.Unsafe
import qualified Data.Map as M
import Data.Array((!),bounds,elems)
import Parameter
import Data
import Vote
import WebObjVotes

insertParams= mapM (uncurry addParam)
                 
  [("percentAprobal" 
        ,Parameter (\[VString p,VString c]->VInt $ unsafePerformIO $ percentAprobal p c ))
  ,("percentReject"
        ,Parameter  (\[VString p,VString c]->VInt $ unsafePerformIO $ percentReject p c ))
  ,("percentNecessary"
        ,Parameter  (\[VString p,VString c]->VInt $ unsafePerformIO $ percentNecessary p c ))
  ,("votationTime"
        ,Parameter (\[VString p,VString c]->VInt $ unsafePerformIO $ votationTime p c ))
  ,("timeSpan"
        ,Parameter  (\[VString p,VString c]->VInt $ unsafePerformIO $ timeSpan p c ))
  ,("PercentNewUser"
        ,Parameter  (\[VString p,VString c]->VInt $ unsafePerformIO $ timeSpan p c ))


  ,("checkApprobal" 
     ,Parameter 
         (\[VSub sub,VArray percents, VMaj majorities]->
               let
                  sumvotes= 100 - percents ! 0
                  lpercents= l+1 where (0,l)= bounds percents
                  noptions= lpercents -2 -- not voted (index 0) and complaints (last index) are the other two
                  percentRejectThis=   percents ! last where (_,last)= bounds percents 
                  percentDesapprovedThis= percents ! 2
                  percentApprobedThis= percents ! 1   

                  check :: (Option, Float)-> Option
                  check ((Option name _), percent) 
                     | percent> sumvotes / fromIntegral noptions= Option name $ Approbed percent  
                     | otherwise=  Option name Processing

                  approbal
                     | sstatus sub /= Processing = VNothing
                     | percentRejectThis > mcomplaint majorities=  VSub sub{sstatus = Rejected $ Unconstitutional percentRejectThis, options =options sub}
                     | otherwise=
                        case options sub of
                         Approbal -> 
                          if percentApprobedThis > mapprobal majorities 
                             then VSub sub{sstatus=Approbed percentApprobedThis,options= Approbal}
                             else VSub sub{sstatus= Rejected (NotEnoughVotes percentApprobedThis ), options= Approbal}
                         ChooseOptions q n opts -> 
                             VSub sub{sstatus=Approbed 0, options= ChooseOptions q n $ map check ( zip opts $ elems percents)}
                  
                in approbal))
    
  ]
