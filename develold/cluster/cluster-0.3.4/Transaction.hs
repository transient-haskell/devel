{-# OPTIONS  -fglasgow-exts -XUndecidableInstances #-}
module Transaction where
import Data.List(union)
import Data.Maybe(catMaybes)

class (Ord id,Eq a)=>Trans t a id | t-> a,t-> id,a-> t,a-> id, id-> t, id-> a where
  apply:: t -> [Maybe a] -> [a]  -- a ticket t applied to the objects [a] to generate new or modified objects

  nullTrans :: t
  -- apply nullTrans as= catMaybes as

  commutative :: t -> Bool       -- commutative transactions can be executed asyncronously
  commutative t = foldr  (&&) True $ map (f t)  all where
      all=  allTransactions
      f t t'=  map (apply1 t . apply1 t' ) as ==  map (apply1 t' . apply1 t) as where
         as =  union ( applyTo t') ( applyTo t')
         apply1 t as= apply t $ map Just as


  allTransactions ::[t]

  getId:: t -> id
  compose:: t -> [t] -> [t]
  compose t ts = t:ts
  applyTo :: t-> [[a]]

  transProperties :: (t,[a]) -> Bool
  transProperties (t, as)
    | commutative t= foldr  (&&) True $ map (f t)  all

    | otherwise = True

    where
        all=  allTransactions
        f t t'= (apply1 t . apply1 t') as == (apply1 t' . apply1 t) as
        apply1 t as= apply t $ map Just as


