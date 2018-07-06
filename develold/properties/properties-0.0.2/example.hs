{-# OPTIONS -fglasgow-exts  -XUndecidableInstances  #-}
module Main where
import Test.Properties
{--------------------------- Example for a Ring  -------
 EXAMPLE for the usage of the package "properties"

 This example show how a class Ring can have attached propeties. these properties
 are tested in the body of two instances of the class as well as in the main procedure
 An additional  operator +++ is defined with awrong version of string concatenation.
 The string property for ++ is applied . The message show the property violation.
-}


class Eq a => Ring a  where
  op1  :: a -> a -> a
  op2  :: a -> a -> a
  zero :: a
  inverse :: a -> a

  ringProperties :: Properties a a a
  ringProperties= properties{

     unary=   [Property "identity"          (\x -> zero `op2` x == zero)
              ,Property "inverse element"   (\x -> x `op1` (inverse x) == zero)]


     ,binary=  [Property "commutative"       (\(x, y) -> x `op1` y == y `op1` x)]


     ,ternary= [Property "associative `op1`" (\(x,y,z) -> (x `op1` y) `op1` z== x `op1` (y `op1` z))
               ,Property "associative `op2`" (\(x,y,z) -> (x `op2` y) `op2` z== x `op2` (y `op2` z))
               ,Property "distributive"      (\(x,y,z) -> x `op2` (y `op1` z)== (x `op2` y) `op1` (x `op2` z))
               ,Property "distributive2"     (\(x,y,z) -> (x `op1` y) `op2` z== (x `op2` z) `op1` (y `op2` z))
               ]
  }

----------------Define a simple Ring instance for Num --------------

instance  Num a  => Ring a where
  op1= (+)
  op2= (*)
  zero = 0
  inverse= negate



data N= N Int deriving ( Eq, Show)


-- a new instance of Ring implicitly defined trough his Num instance
-- the check of binary properties is not exhaustive. It is there just to show how to do it
instance Show N => Num N where
  (+) (N x) (N y)= N (x + y)  `verify`  binary ringProperties `with` ((N x), (N y))
  (-) (N x) (N y)= N (x - y)
  (*) (N x) (N y)= N (x * y)
  negate (N x)= N (-x)
  abs (N x) = N (abs x)
  signum (N x)= N (signum x)
  fromInteger i= N (fromInteger i)


-- a wrong string concatenation operator

(+++)  x  y = x++y++"hello"


stringProperties=
    properties {binary=[Property "length" (\(x, y)-> length (x+++y)== length x + length y)]}


quickCheckProperty :: N -> Bool
quickCheckProperty =  \x -> x  == x

main= do
  
  let x= N 5 
  let y= N 3        `verify`  unary ringProperties `with` x

  let z= x + y      `verify` [Property "quickChekStyleProperty" quickCheckProperty] `with` y
  print z
  let t= N 7
  print t           `verify`  ternary ringProperties `with` (x,y,z)

  let a= "a"
  let b= "b"
  print $ a +++ b   `verify`  binary stringProperties `with` (a,b)    --will fail
  print x
  

