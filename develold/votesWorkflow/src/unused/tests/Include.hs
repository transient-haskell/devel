module Include where
{-
data Int1= Int1 Int deriving Show

sum1 :: [Int1  ] -> Int1
sum1 [Int1 x,Int1 y]= Int1 $ x+y
-}
data A= A{b :: Int}


sum2 [A x,A y]= x+y
