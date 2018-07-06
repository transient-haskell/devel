{-# OPTIONS -fglasgow-exts  #-}

{-
http://hackage.haskell.org/cgi-bin/hackage-scripts/package/simple-reflect

-}
module DynAlgebra where

data Dyn= forall a.(Num a, Eq a, Show a, Ord a, Read a, Simplify a)=> Dyn a

instance Show Dyn where
   show (Dyn a)= show a

instance Eq Dyn where
    (Dyn a) == (Dyn b)= solve a== solve b

class Simplify a where
  simplify :: a -> a
  solve    :: a -> Polonomial Expr1

data Polonomial e  =  e  :/ e  | e :+ e  | e :* e    deriving Show

data Expr1= Var String | I Integer   | Pi | E   deriving Show

-- Integer :/ Integer for exact rational aritetic

data Expr= Polinomial Expr  | Formula Dyn deriving (Show, Eq)


instance Num (Polonomial Expr1) where
 fromInteger = I
 (+) (I a) (I b)= I (a+b)
 (+) e1 e2  = e1 :+ e2

 (*) (I a) (I b)= I (a*b)
 (*) e1 e2 = (:*) e1 e2
 
 abs (I x) | x>=0 = I x 
           | otherwise = I (-x)
 signum (I x) | x >= 0 = 1 
              | otherwise= -1

instance Ord Polonomial  where
 compare  (I a) (I b)= compare a b
 compare  ( x :+ z)  y | x < y && z >0 = LT
                       | x > y && z >0 = GT

 compare   y ( x :+ z) | y > x && z >0 = LT
                       | y > x && z >0 = GT

instance Eq Polonomial  where
   (I a) == (I b) = a == b

   
   (I a :+ I b)== ( I c :+ I d)= a + b== c + d
   (I a :* I b)== ( I c :* I d)= a * b== c * d

   (a :+ b)== (c :+ d)= a==c && b == d
   (a :* b)== (c :* d)= a== c && b == d
   (a :/ b)== (c :/ d)= a * d == b * c
   
   exp1 == exp2 = simplify exp1== simplify exp2

            
             
instance Simplify  (Polonomial Expr1) where
  solve  x = simplify x     

  simplify ( (I x) :/ (I y))=  case quotRem x y   of
                      (q,0) -> I q
                      (q,r) -> let m= mcd y r in (I(x `div` m)) :/  (I(y `div` m)) 
 
  simplify ((I a) :+ (I b))= I (a + b)
  simplify ((I a) :* (I b))= I (a * b)
  
  
  simplify ((a :* b) :+ (c :* d)) | a == c = simplify $  a * (b :+d)
  simplify (exp1 :+ exp2) = simplify exp1 :+ simplify exp2
  simplify (exp1 :* exp2) = simplify exp1 :* simplify exp2

  simplify expr= expr

mcd x y= case mod x y of
           0 -> y
           t -> mcd y t
  
  
subst:: Polonomial  -> [(String, Polonomial )] -> Polonomial 
subst exp l= subs1 exp where
 subs1 (Var v)= case lookup v l of
                 Nothing -> Var v
                 Just e  -> e
 subs1 (e1 :+ e2) = ((subs1 e1) :+ (subs1 e2))
 subs1 (e1 :* e2) = ((subs1 e1) :* (subs1 e2))
 subs1 (e1 :/ e2) = ((subs1 e1) :/ (subs1 e2))

 subst e= e

f x= x :* x



main= print  $ solve  $ 2 :+1


