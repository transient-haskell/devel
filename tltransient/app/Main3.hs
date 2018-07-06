{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
MultiParamTypeClasses, FlexibleInstances, PolyKinds,
FlexibleContexts, UndecidableInstances, ConstraintKinds,
ScopedTypeVariables, RebindableSyntax, GeneralizedNewtypeDeriving #-}

import Prelude  hiding (return, (>>=), (>>))
import qualified Prelude as P (return, (>>=)) 
import qualified Control.Applicative as A
import qualified Transient.Base as Tr
import qualified Transient.Indeterminism as In
import Data.Typeable
import qualified Control.Monad.Trans as T
import Data.Type.Set
import Data.Type.Bool 
import Data.Type.Equality
import qualified Data.Monoid as M

data IOEff
data State a 
data AllStates
data EarlyTermination 
data Async -- may produce a response at a later time
data Streaming 
data MThread  -- Multi-threaded
data Cloud
data ThrowException e

--data Mode = Streaming | MThread | Async

type family CmpModes a  b :: Ordering where
            CmpModes Async MThread = 'LT
            CmpModes Async Streaming     = 'LT
            CmpModes Async IOEff         = 'LT

            CmpModes MThread Streaming    = 'LT
            CmpModes MThread (State a)    = 'LT

            CmpModes IOEff MThread        = 'LT
            CmpModes IOEff (State a)            = 'LT
            CmpModes Streaming (State a)        = 'LT

            CmpModes (State a) Async     = 'LT
            CmpModes (State a) (State b)        = 
                If (CmpModes b a == 'LT) 'GT (CmpModes b a)
            CmpModes a a                        = 'EQ
            CmpModes a b                        =
               If (CmpModes b a == 'LT) 'GT (CmpModes b a)
            

type instance Cmp a b = CmpModes a b

-- type family CmpNodes a b


newtype TR eff req m a= TR  (m a)  -- deriving(A.Applicative, A.Alternative)

--instance (Monad m) => Monad (TR eff m) where


-- newtype TRM eff  a= MR (Tr.TransIO a) deriving(Monad, Functor, A.Applicative, A.Alternative)



type TRS effs req  a = TR (Set effs) (Set req) Tr.TransIO a 

instance T.MonadTrans (TR eff req) where
    lift = TR

--instance MonadIO m => MonadIO (TR eff m) where
liftIO  :: IO a -> TRS '[IOEff] req a
liftIO = TR . T.liftIO

instance Functor m => Functor (TR eff req m) where
    fmap f (TR x)= TR $ fmap f x 

coerceEffs :: TR effs req m a -> TR effs' req m a
coerceEffs (TR comp)= TR comp

empty ::  TRS '[EarlyTermination] req a
empty= TR A.empty

(<|>) ::  TRS effs req a -> TRS effs' req a -> TRS (Union (effs :\ EarlyTermination) effs') req a
TR a <|> TR b=  TR $  a A.<|>  b


(<*>) ::   TRS effs req (a -> b) -> TRS effs' req a 
      -> TRS (Union effs  effs' :\ Async) req b
TR a <*> TR b=  TR $  a A.<*>  b

mempty :: Monoid a => TRS '[] req a
mempty = TR M.mempty

(<>) ::  Monoid a => TRS effs req a -> TRS effs' req a 
      -> TRS (Union effs  effs')  req  a
TR a <> TR b=  TR $  a M.<>  b

return :: a -> TRS '[] req a
return= TR. P.return 

(>>=) :: TRS effs req a 
      -> (a -> TRS effs' req b) 
      -> TRS  (Union effs  effs') req   b
(TR a) >>= b = TR $ a P.>>= \x -> let  TR z= b x in z  

(>>) ::  TRS effs req a 
     -> TRS effs' req b 
     -> TRS  (Union effs effs') req b
a >> b = a >>= \_ -> b

-- keep :: Typeable a => TR ( Set '[Async, Streaming, MThread 
--            , EarlyTermination, HasIO, Pure]) Tr.TransIO a -> IO (Maybe a)
keep :: Typeable a => TRS (  effs) req a -> IO (Maybe a)
keep (TR x) = Tr.keep x 



getd :: (Typeable a) => a -> TRS '[State a] req a
getd x = TR $ Tr.getSData A.<|> P.return x

set :: Typeable a => a -> TRS  '[State a] req ()
set = TR . Tr.setData 

get :: (Typeable a, Member (State a) req) =>  TRS '[] req a
get = TR $ Tr.getSData

-- unsetStates :: TRS (req :\ AllStates) ()
-- unsetStates= undefined

tests= keep $ do 
   n <- get
   liftIO $ print (n :: Int)
   set (2 :: Int)
   testState 

testState :: TRS '[IOEff] '[State Int] ()
testState=  do 
     n <- get 
     liftIO $ print (n :: Int)

threads0 :: TRS effs req a -> TRS ( effs :\ MThread) req a
threads0 (TR x)= TR $ Tr.threads 0 x



type family FilterState (xs :: [k]) :: [k] where
    FilterState (xs) = ( (Filter FMin AllStates xs))  :++ ( (Filter FMax AllStates xs))

test :: Set (Filter FMin Async '[Async])  
test= undefined

main= keep $ do 
    set "hello"
    async (P.return "hello")
    r <- getd "world"
    liftIO $ putStrLn r

-- prs :: TRS ( '[State String]:\ AllStates) ()
-- prs = undefined

collect :: (Member Streaming effs) => Int -> TRS effs req  a 
        -> TRS (effs :\Async :\MThread :\ Streaming) req  [a]
collect n (TR x)= TR  $ In.collect n x

async ::  IO a -> TRS '[Async,MThread] req a
async = TR . Tr.async 

waitEvents :: IO a -> TRS  '[Streaming,MThread] req a
waitEvents = TR . Tr.waitEvents

main1= keep  test1

test1= do
        set "hello"

        r <- collect 2 $ do  async (P.return "hello") <|> waitEvents (P.return "world")
        liftIO $ print r