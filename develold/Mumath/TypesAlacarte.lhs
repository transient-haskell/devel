> {-# LANGUAGE OverlappingInstances,MultiParamTypeClasses,TypeOperators,UndecidableInstances,IncoherentInstances,FlexibleInstances,FlexibleContexts  #-}

Here are the definitions from Wouter's paper, which cleverly manage
the instances of (:<:)

> data (f :+: g) a = Inl (f a) | Inr (g a)
> infixr 6 :+:

> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap f (Inl x) = Inl (fmap f x)
>   fmap f (Inr x) = Inr (fmap f x)

> class (Functor sub, Functor sup) => sub :<: sup where
>   inj :: sub a -> sup a

> instance Functor f => (:<:) f f where
>   inj = id

> instance (Functor f, Functor g) => (:<:) f (f :+: g) where
>   inj = Inl

> instance (Functor f, Functor g, Functor h, (f :<: g)) => (:<:) f (h :+: g) where
>   inj = Inr . inj

Now in my case, I want to be able to inject an arbitrary coproduct
into a larger one, assuming the components are in the same order, so
it is a subsequence of the larger sum, for example:

> coprodInject1 :: (Functor f, Functor g, Functor h, Functor i) => (f :+: g :+: h) a -> (f :+: g :+: h :+: i) a
> coprodInject1 = inj

> coprodInject2 :: (Functor f, Functor g, Functor h, Functor i) => (f :+: g :+: i) a -> (f :+: g :+: h :+: i) a
> coprodInject2 = inj


> instance (Functor f, Functor g, Functor h, (g :<: h)) => (:<:) (f :+: g) (f :+: h) where
>   inj (Inl x) = Inl x
>   inj (Inr x) = Inr (inj x)

The above works fine, and maybe I could manipulate my code to only
have injections
of those forms, but I want this next one to work too:

> coprodInject3 :: (Functor f, Functor g, Functor h, Functor i) => (f :+: g :+: i :+: j) a -> (f :+: g :+: h :+: i :+: j) a
> coprodInject3 = inj