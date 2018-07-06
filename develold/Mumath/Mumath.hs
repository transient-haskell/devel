{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances
-fallow-overlapping-instances #-}
module Mumath where
import TypesAlaCarte
import Prelude  

instance (Eq (Expr f), Show (f (Expr f)),(Add :<: f), (Mul :<: f)) => Num (Expr f) where
   e + e'= add e e' 
   e * e'= mul e e'
   --abs
   --signum
   --fromInteger i= In $ inj $ Val i

--necesario
class Functor f => Solve f where
    solveA :: f (Expr e) -> (Expr e)

instance (Solve f, Solve g) => Solve (f :+: g) where
    solveA (Inl f) = solveA f
    solveA (Inr g) = solveA g

    
solve :: Solve f => Expr f -> Expr f
solve expr = foldExpr solveA expr




data Sin e= Sin e deriving Show

instance Functor Sin where
    fmap f (Sin x) = Sin (f x)
    

instance Solve Sin where
    solveA (Sin (In (Mul( Mul 2 x)))) = solveA $ 2 * (sine x) * (sine x)
    

sine :: (Sin :<: e) =>  Expr e -> Expr e
sine x = In $ inj $ Sin x
        
{-
Expr= Pi | Number Integer Integer | Expr :+: Expr | Expr :*: Expr

solve e =do
  rules <- readMVar tvrules
  map rules e >>= filter strategy e  



data Val e = Val Int deriving Show
data Add e = Add e e deriving Show
data Mul e = Mul e e deriving Show

instance Functor Val where
    fmap f (Val x) = Val x
instance Functor Add where
    fmap f (Add l r) = Add (f l) (f r)
instance Functor Mul where
    fmap f (Mul l r) = Mul (f l) (f r)


instance Eval Val where
    evalA (Val x) = x
instance Eval Add where
    evalA (Add x y) = x + y
instance Eval Mul where
    evalA (Mul x y) = x * y



eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalA expr

val :: (Val :<: e) => Int -> Expr e
val x = In $ inj $ Val x

add :: (Add :<: e) => Expr e -> Expr e -> Expr e
add x y = In $ inj $ Add x y

mul :: (Mul :<: e) => Expr e -> Expr e -> Expr e
mul x y = In $ inj $ Mul x y

-}