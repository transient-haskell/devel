{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances
-fallow-overlapping-instances #-}
module TypesAlaCarte where
--Implementation by Ryan Ingram ryani.spam at gmail.com . 
--Based on:
--Wouter Swierstra. Data Type a la Carte

newtype Expr f = In (f (Expr f))

instance Show (f (Expr f)) => Show (Expr f) where
    showsPrec _ (In x) = showParen True (showString "In " . showsPrec 11 x)

out :: Expr f -> f (Expr f)
out (In x) = x

data (f :+: g) e = Inl (f e) | Inr (g e) deriving Show

instance (Functor f, Functor g) => Functor (f :+: g) where
   fmap h (Inl f) = Inl (fmap h f)
   fmap h (Inr g) = Inr (fmap h g)

class (Functor sub, Functor sup) => (:<:) sub sup where
   inj :: sub a -> sup a

instance TypTree sub sup => (:<:) sub sup where inj = treeInj

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
    evalA :: f Int -> Int

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalA (Inl f) = evalA f
    evalA (Inr g) = evalA g

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalA expr

--
-- TypTree.  This is basically the same as (:<:).
-- I kept it a separate class during development.  This also allows the use of
-- additional constraints on (:<:) which improve the error messages
-- when you try,
-- for example:
--   testBroken :: Expr (Mul :+: Add)
--   testBroken = val 3
--
class (Functor sub, Functor sup) => TypTree sub sup where
    treeInj :: sub a -> sup a
instance Functor x => TypTree x x where
    treeInj = id

--
-- The magic all happens here
-- We use "IsTreeMember" to determine if a type is part of a tree with leaves
-- of various types and internal nodes of type (l :+: r).
--
class IsTreeMember (sub :: * -> *) (sup :: * -> *) b | sub sup -> b

instance TypEq x y b => IsTreeMember x y b
instance (IsTreeMember x l bl, IsTreeMember x r br, TypOr bl br b) =>IsTreeMember x (l :+: r) b

class (Functor sub, Functor l, Functor r) => TypTree' b sub l r where
    treeInj' :: b -> sub a -> (l :+: r) a

--
-- We can then use this result to decide whether to select from the
-- left or the right.
--
instance (TypTree x l, Functor r) => TypTree' HTrue x l r where
    treeInj' _ = Inl . treeInj
instance (TypTree x r, Functor l) => TypTree' HFalse x l r where
    treeInj' _ = Inr . treeInj

--
-- Finally, this allows us to select which treeInj' to use based on the
-- type passed in.
-- 
instance (IsTreeMember x l b, TypTree' b x l r) => TypTree x (l :+: r) where
    treeInj = treeInj' (undefined :: b)

class TypOr b1 b2 res | b1 b2 -> res
instance TypOr HFalse HFalse HFalse
instance TypOr HFalse HTrue  HTrue
instance TypOr HTrue  HFalse HTrue
instance TypOr HTrue  HTrue  HTrue

-- Type equality, semi-lifted from the hlist paper; this only works in GHC.
--
-- You can avoid the reliance on GHC6.8 type equality constraints
-- by using TypeCast from the HList library instead.
--
-- see http://www.okmij.org/ftp/Haskell/types.html#HList
-- for the source of this idea.

data HFalse
data HTrue

class TypEq (x :: * -> *) (y :: * -> *) b | x y -> b
instance TypEq x x HTrue
instance (b ~ HFalse) => TypEq x y b
-------------------------------------------------------------------------------------------------------


-- EXAMPLE



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




val :: (Val :<: e) => Int -> Expr e
val x = In $ inj $ Val x

add :: (Add :<: e) => Expr e -> Expr e -> Expr e
add x y = In $ inj $ Add x y

mul :: (Mul :<: e) => Expr e -> Expr e -> Expr e
mul x y = In $ inj $ Mul x y

test :: Expr (Val :+: Add)
test = In (Inr (Add (val 118) (val 1219)))

test2 :: Expr (Add :+: Val)
test2 = val 1

test3 :: Expr ((Add :+: Val) :+: Mul)
test3 = add (mul (val 1) (val 2)) (val 3)

test4 :: Expr (Add :+: (Val :+: Mul))
test4 = add (mul (val 1) (val 2)) (val 3)

-- our typtree selection prefers left injection
test5 :: Expr ((Val :+: Val) :+: (Val :+: Val))
test5 = val 1


