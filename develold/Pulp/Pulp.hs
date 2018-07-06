import qualified MFlow.Forms as Forms
import Control.Applicative
import Control.Monad.State

data Weaver= (String, Html)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y

caseExp :: [a -> b] -> a -> b
caseExp (f:fs) x=
  catch (f x) (\(e :: PatternMatchFail) -> caseExp fs x)



data Pulp v m a= Pulp String (Forms.View v m a)

wlink x s=  Pulp ("wlink "++ show x ++ " " ++ show s) $ Forms.wlink x s



instance (Functor m, Monad m) => Functor (Pulp v m) where
   fmap f (Pulp s w)= Pulp s  $ fmap f w

instance (Forms.FormInput v, Monad m, Functor m) => Applicative (Pulp v m) where
   pure = Pulp "" . return
   g <*> f= let Pulp s w = g
                Pulp s' w'= f
            in  Pulp (s ++ s') $ w <*> w'



instance Alternative (Pulp v m) where
   g <|> f = let Pulp s x = g
                 Pulp s' x'= f
             in  Pulp (s ++ " <|> " ++ s') $ x <|> x'


instance Monad (Pulp v m) where
   g >>= f = let Pulp s1 w1 = g
             in  Pulp s1 $ View $ do
               FormElm v mx <- runView w
               case mx of
                 Nothing -> return FormElm v Nothing
                 Just x -> do
                    Pulp s' w' <- f x
                    Pulp (s ++ s')  w'
   return = pure

inBrowser (Pulp s x)= s

main= do
  print $ inBrowser $ do x <- return 3 ; wlink x x
  print $ inBrowser $ wlink 2 3 <|> wlink 3 4



