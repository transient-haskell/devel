module Main where
import Debug.Trace
--import Control.Parallel.Strategies
debug a b = trace b a

-- MINIMAL code for forcing a strict evaluation of all the list members in a list
-- TAKEN FROM Control.Concurrent.Strategies
  
--type Done = ()

-- | A strategy takes a value and returns a 'Done' value to indicate that
--   the specifed evaluation has been performed.
--type Strategy a = a -> Done

-- | Reduces its argument to weak head normal form.
--rwhnf :: Strategy a 
rwhnf x = x `seq` ()

--using :: a -> Strategy a -> a
using x s = s x `seq` x

-- | Sequentially applies a strategy to each element of a list.
-- seqList :: Strategy a -> Strategy [a]
seqList strat []     = ()
seqList strat (x:xs) = strat x `seq` (seqList strat xs)
         
strictListEval= seqList rwhnf

main= do
   print "lazy evaluation"
   let a=   [ trace "1"  1, trace "2"  2, trace "3"  3] 
   let b=   [ trace "1'"  1, trace "2'"  2, trace "3'"  3] 

   print $ zip a b

   print "Strict evaluation"
   let a=   [ trace "1"  1, trace "2"  2, trace "3"  3] `using` strictListEval
   let b=   [ trace "1'"  1, trace "2'"  2, trace "3'"  3] `using` strictListEval

   print $ zip a b
  
