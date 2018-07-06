{-# OPTIONS  -fglasgow-exts -fallow-undecidable-instances #-}
module Transaction where


class (Ord id)=>Trans t a id | t->a,t->id,a->t,a->id, id->t, id->a where
  apply:: t -> [Maybe a] -> [a]  -- a ticket t applied to the objects [a] to generate new or modified objects
  commutative :: t -> Bool       -- commutative transactions can be executed asyncronously   
  commutative _ = True                     
  getId:: t -> id
  compose:: t -> [t] -> [t]
  compose t ts = t:ts    


  


