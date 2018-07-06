{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- some imports
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import GHC.Conc
import System.IO.Unsafe
import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as M 
import Data.Typeable
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State 
import Data.Monoid
import Unsafe.Coerce
import Control.Exception hiding (onException)
import Data.Maybe

-- convenient debug operator
import Debug.Trace
x !> y=  trace (show y) x
infixr 0 !>

{-
The Monad class:

class Monad m where
   return :: m a
   (>>=) :: m a -> (a -> m b) -> m b

The second term of (>>=) is a lambda. It is also a kind of continuation. 
It could be seen also as a callback: 
(>>=) can be read as this: when 'm a' (the first term) is executed, apply the second term as a callback
which will receive the result of the first term.

do x <- mx
   y <- my
   z

-- ... is equivalent to:
do x <- mx
   do y <- my
      z

-- ... desugars to:
mx >>= (\x ->
              my >>= (\y ->
                             z ))

However we want to define 'mx' 'my'   etc as compuatations that know their continuations.
In a 'normal' monad each computation does not know what's next.
-}

{- This is the original Continuation Monad

In the continuation monad, each computation 'mx'... is a lambda  whose parametet 'a -> m r' 
is the continuation 'c', in which 'a' is the value returned by the previous term.

-}

newtype Cont' r m a = Cont'{runCont' :: (a -> m r) -> m r}

instance Functor (Cont' r m) where
    fmap f m = Cont' $ \ c -> runCont' m (c . f)

instance Applicative (Cont' r m) where
    pure x  = Cont' (\c ->c x) -- Cont ($ x)
    f <*> v = Cont' $ \ c -> runCont' f $ \ g -> runCont' v $ \t -> c $ g t

instance Monad (Cont' r m) where
  return x = Cont' (\c -> c x) -- Cont ($ x)
  -- (>>=) :: Cont r m a -> (a -> Cont r m b) -> Cont r m b
  m >>= k  = Cont' $ \ c -> runCont' m $ \ x -> runCont' ( k x ) c

  -- in the continuation monad, each term know what is after it, his continuation "c". 
  -- This allows for the programming of powerful effects as we will know below. 
  -- 'x' is of type 'a'
  -- 'm' is of type 'Cont r m a' 
  -- However the Cont monad is weird. the type of the result 'r' depend on a term that is outside of the
  -- computation itself, since  'Cont r m b` === Cont ((b -> m r) -> m r)
  -- it does not materialize in a result. It needs a final lambda 'b -> m r' to produce a value 'm r'


-- State being carried out by the monad. many of these fields are not used here.
type SData= ()

data LifeCycle = Alive | Parent | Listener | Dead
  deriving (Eq, Show)

-- | Stat describes the context of a ContIO computation:
data Stat = Stat
  { mfData      :: M.Map TypeRep SData
    -- ^ State data accessed with get or put operations
  , mfSequence  :: Int
  , threadId    :: ThreadId
  , freeTh      :: Bool
    -- ^ When 'True', threads are not killed using kill primitives

  , parent      :: Maybe Stat
    -- ^ The parent of this thread

  , children    :: MVar [Stat]
    -- ^ Forked child threads, used only when 'freeTh' is 'False'

  , maxThread   :: Maybe (IORef Int)
    -- ^ Maximum number of threads that are allowed to be created

  , labelth     :: IORef (LifeCycle, BS.ByteString)
    -- ^ Label the thread with its lifecycle state and a label string
  , emptyOut    :: Bool
    -- ^ Used by empty
  } deriving Typeable

-- to avoid a further lambda (b -> m r) to get a result 'r',
-- Type coercion is necessary, since continuations can only be modeled fully within Indexed monads. 
-- See paper P. Wadler "Monads and composable continuations" 
--                http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.94.985&rep=rep1&type=pdf
-- The symtom of that problem in the typical continuation monad is the extra parameter r that complicates
--  reasoning. The monad below eliminates the extra parameter by coercing types since, by construction, 
-- the  parameter 'x' of the second term is of the type of the first term of the bind.
ety :: a -> b 
ety=   dontWorryEverithingisOk
tdyn :: a -> Dyn
tdyn=  dontWorryEverithingisOk
fdyn :: Dyn -> a
fdyn = dontWorryEverithingisOk

dontWorryEverithingisOk= unsafeCoerce

type Dyn= ()

-- the new data definition:

newtype Cont m a = Cont { runContT :: (Dyn -> m a) -> m a }

-- will carry on an state. use the state monad transformer
type StateIO = StateT Stat IO

-- load our final monad with the payload state we defined above.
type ContIO  = Cont  StateIO 

instance Monad ContIO  where
    return  = pure  --  Cont ($ tdyn a) ===  Cont (\c -> c $ tdyn a)


    -- (>>=) ContIO a -> (a -> ContIO b) -> ContIO b 
    -- (>>=) Cont ((Dyn -> m a) -> m a) -> (a -> Cont ((Dyn -> m b )) -> m b )
    m >>= k  = Cont $ \c -> ety $ runContT m (\x -> ety $ runContT ( k $ fdyn x) c)

-- the instance, type coercions apart, is identical to the standard continuation. BUT...
-- now we know that the result is the result of the second term. a further lambda/continuation/callback 
-- is not needed

instance MonadState  Stat ContIO where
    get= lift get
    put= lift . put

instance MonadTrans (Cont ) where
    lift m = Cont ((ety m) >>=)

instance MonadIO ContIO  where
    liftIO = lift . liftIO

callCC :: ((a -> Cont  m b) -> Cont  m a) -> Cont  m a
callCC f = Cont $ \ c -> runContT (f (\ x -> Cont $ \ _ ->  ety $ c $ tdyn x)) c


-- run the monad with an state 'st'
runContState :: Stat -> ContIO  a ->  IO ( a, Stat)
runContState st t= runStateT  (runcst t) st 
  where  
  runcst :: ContIO a -> StateIO  a
  runcst t=   runContT  t  (return . ety id )

-- | Run a Cont computation with a default initial state
runCont :: ContIO a -> IO ( a, Stat)
runCont t = do
    th     <- myThreadId
    label  <- newIORef $ (Alive, BS.pack "top")
    childs <- newMVar []
    runContState (emptyStat th label childs) t
    where
    emptyStat :: ThreadId -> IORef (LifeCycle, BS.ByteString) -> MVar [Stat] -> Stat
    emptyStat th label childs =
        Stat { mfData     = mempty
               , mfSequence = 0
               , threadId   = th
               , freeTh     = False
               , parent     = Nothing
               , children   = childs
               , maxThread  = Nothing
               , labelth    = label
               , emptyOut   = False }

-- executing the continuation in CallCC with a value is executing the contination with this value. 
-- the rest of the callCC block is ignored 
callCCTest= runCont $ do

    r <- callCC $ \ret -> do 
        ret  100
        liftIO  $ print "hello"    -- we are here in the Cont/Cont monad
        return 1
    liftIO $ print r
    liftIO $ print "world"

-- > main= callCCTest
--  100
--  "world"

callCCTest1= runCont $ do 
    r <- Cont $ \ret -> do 
            ret $ tdyn 100         -- we are here in the state monad
            liftIO $ print "hello"
            return 1
    liftIO $ print $ r
    liftIO $ print $ "world"

-- > main= callCCTest1
-- 100
-- "world"
-- "hello"

-- empty means the early finalization of a computation and the execution of a possible alernative computation
-- This is implemented by a Empty exception which carries out a computation state.
-- this exception can be catched by the alternative computation, which continue the execution.

newtype Empty= Empty Stat deriving Typeable
instance Show Empty where show _= "Empty"
instance Exception Empty

instance  Alternative ContIO where
  empty= get >>= \st -> liftIO . throw $ Empty st

  f <|> g= callCC $ \k -> do
      -- -- The straigh definition: invoke f and the continuation. if it fails with empty run g followed 
      -- -- by the continuation. The state 'st' is propagated to the result.
      -- -- Since exceptions are defined in the IO Monad, we need to run them naked in IO, using
      -- -- 'runContState', and wear the result again with 'liftIO' 
 
      --  st <- get
      --  liftIO $ (runContState st (f >>= k) 
      --                   `catch` (\(Empty st) -> runContState st (g >>= k) ))   
      --  empty

      -- -- however empty exception in the continuation of f in 'f >>= cont' would trigger the execution of g
      -- -- This is not what is needed. we need it only when Empty is triggered in 'f'
      --
      -- A state variable `emptyOut` is used to detect when empty is called in the continuation
      -- In this case, empty is ignored and retrown.
       st <- get
       
       liftIO $ io st f k `catch` \(Empty st) -> do
                    let c =  emptyOut  st
                    when c $ throw (Empty st)  -- when the exception comes from the continuation do not execute g
                    io st g k
       empty

       where
       io st f cont= runContState st{emptyOut=False}  (f >>= cont' ) 
               where cont' x= do modify $ \st ->st{emptyOut=True} ; cont x

instance Functor (Cont  m) where
    fmap f m = Cont $ \c -> ety $ runContT m $ \ x -> ety c $ f $ fdyn x


instance Applicative ContIO  where
    pure a  = Cont ($ tdyn a)
    -- -- this would be  the standard definition following the continuation monad
    --f <*> v = ety $ Cont $ \ k -> ety $ runContT f $ \ g -> ety $ runContT v $ \t -> k $ (ety g) t
    
    -- but we need to give the opportunity to execute both terms in parallel
    -- so we define it as the composition of two alternative computations
    f <*> v =   do
          r1 <- liftIO $ newIORef Nothing
          r2 <- liftIO $ newIORef Nothing
          (fparallel r1 r2)  <|> (vparallel  r1 r2)
      where
      -- to allow parallel execution of both terms, two mutable variables store the result of each term
      -- one executes f, the other v. 
      -- Each term inspect if the other has finished
      --          if it has not finished, trows empty and the thread finish
      --          if has finished, evaluate the result and execute the continuation `k` with the result
      fparallel :: IORef (Maybe(a -> b)) -> IORef (Maybe a) -> ContIO b 
      fparallel r1 r2= ety $ Cont $ \k  -> 
          runContT f $ \g -> do
                (liftIO $ writeIORef r1  $ Just (fdyn  g))
                mt <- liftIO $ readIORef r2               
                case mt of 
                  Just t -> k $ (fdyn g) t
                  Nothing -> get >>= liftIO . throw . Empty 
                  
      vparallel :: IORef (Maybe(a -> b)) -> IORef (Maybe a) -> ContIO b
      vparallel  r1 r2=  ety $ Cont $ \k  ->  
          runContT v $ \t ->  do
                 (liftIO $ writeIORef r2 $ Just (fdyn t)) 
                 mg <- liftIO $ readIORef r1            
                 case mg of 
                   Nothing -> get >>= liftIO . throw . Empty  
                   Just g ->  k $ (ety g) t 

-- standard monoid definition. Since is defined in terms of Applicative, it allows parallel execution
-- of x and y.
instance Monoid a => Monoid (ContIO a) where
  mappend x y = mappend <$> x <*> y
  mempty      = return mempty

-- An "algebra" defined in terms of Applicative, so it allows reactive and parallel execution! 
-- (as well as sequential) as we will see below
instance (Num a, Eq a) => Num (ContIO a) where
    fromInteger = return . fromInteger
    mf + mg     = (+) <$> mf <*> mg
    mf * mg     = (*) <$> mf <*> mg
    negate f    = f >>= return . negate
    abs f       = f >>= return . abs
    signum f    = f >>= return . signum




-- How is this parallel execution permitted?  Because we can create threads that execute continuations
-- these threads execute all alternative computations in parallel.
-- since all combinators are constructes in terms of the Applicative operators which are parallel enabled.
--
-- async execute an 'io' operation, then create a thread and  initiates the execution of the continuation within it.
-- then it leaves the current thread with 'empty' so an alternative computation can use this thread.
async :: IO a -> ContIO a 
async io= callCC $ \ret -> do
              st <-  get
              liftIO $ forkIOE $ do
                        runContState st ( liftIO io >>= ret )  `catch` exceptBack st
                        return ()
              empty 

forkIOE x= forkIO $ (x >> return ())  `catch` \(Empty _)-> return ()  

-- the initiator of the monad. since threads may die with an empty exception,
-- we need to keep the program running even if the only thread running is the
-- console loop

mexit= unsafePerformIO $ newEmptyMVar  
keep mx= do
    forkIOE $( runCont mx  >> return ()) `catch` \(Empty _) -> return ()
    takeMVar mexit  

-- do the same than 'async' but execute the 'io' operation in a loop. each time it creates a new thread.
waitEvents :: IO a -> ContIO a
waitEvents io= callCC $ \ret -> do
      st <- get 
      loop ret st 
      where
      loop ret st=  do
              liftIO $ forkIOE $ do 
                        runContState st  (liftIO io >>=  ret ) `catch` exceptBack st
                        return () 
              loop ret st

testWaitEvents= do 
  r <- waitEvents getLine 
  liftIO $ putStr "received: " >> print r

-- > main= keep testWaitEvents
-- > hello
-- received: "hello"
-- > world
-- received: "world"
-- > ssds
-- received: "ssds"

-- asynchronous programs combine algebraically with (<|>)

-- An IRC client:
--
-- main = do
--  h <- withSocketsDo $ connectTo "irc.freenode.net" $ PortNumber $ fromIntegral 6667
--  keep' $ (waitEvents getLine >>= liftIO . hPutStrLn h) <|> 
--          (waitEvents (hGetLine h) >>= liftIO . putStrLn )


-- choose execute as many alternative 'async' operation as there are values in a list.
-- So each value is returned to the continuation, which is executed in a different thread.
-- So it executes the rest of tjhe computation for all the values in parallel.
-- this initiates a parallel non-deterministic computation.
--
-- it is equivalent to  'async(return x1) <|> async(return x2)....`

choose :: [a] -> ContIO a
choose xs = foldl (<|>) empty $ map (async . return) xs



choosetwo=  do 
  r  <- choose [1..3]
  r' <- choose ['a'..'c']
  th <- liftIO  myThreadId
  liftIO $ print (r,r', th)

-- > main= keep choosetwo 
-- (2,'a',ThreadId 79)
-- (2,'b',ThreadId 80)
-- (1,'a',ThreadId 78)
-- (1,'b',ThreadId 82)
-- (1,'c',ThreadId 83)
-- (2,'c',ThreadId 84)
-- (3,'a',ThreadId 85)
-- (3,'b',ThreadId 86)
-- (3,'c',ThreadId 87)

pythagoras =  do
  x <- choose [1..10]
  y <- choose ([1 .. x] :: [Int])
  z <- choose [1 .. round $ sqrt(fromIntegral $ 2*x*x)]

  guard (x*x+y*y==z*z)
  th <- liftIO  myThreadId
  liftIO $ print (x, y, z, th)

-- main= keep  pythagoras
-- (4,3,5,ThreadId 173)
-- (8,6,10,ThreadId 565)

-- A continuation is a callback. We can cheat a callback handler by giving it our continuation.
--
-- So instead of
--
--  > do
--  >    ....
--  >    setCallback ourCallback -- our logic is interrupted here
--
--  > ourCallback value= do 
--  >      foo value; ..... -- ... and continues here
--  >      ...
--
--  Instead of that, now we can write:
--
--  > do
--  >    ....
--  >    value <- react setCallback  (return ())
--  >    foo value                           -- code is not broken
--  >    ...

react 
  :: ((eventdata ->  IO response) -> IO ())
  -> IO  response
  -> ContIO eventdata
react setCallback iob= callCC $ \ret -> do
            st <- get
            liftIO $ setCallback $  \x ->  do 
                  runContState st (ret x) `catch` exceptBack st
                  iob
            empty

   


-- react used for console input

-- Let's define a framework with callbacks. 
--
-- Since we may have many threads/modules/programs, I can create a thread which will input from the keyboard.
-- Other modules, probably running in different threads may set callbacks which this console input thread 
-- could call when some input line is entered.

rcb= unsafePerformIO $ newIORef M.empty 

setCallback :: String -> (String ->  IO ()) -> IO ()
setCallback name cb= atomicModifyIORef rcb $ \cbs ->  (M.insert name cb  cbs,())

delCallback name= atomicModifyIORef rcb $ \cbs -> (M.delete name cbs,())


-- `reactOption` set one of these callbacks, when the input matches the string `resp` then
-- it returns 'resp', so the rest of the computation is executed.
-- otherwise, empty stops from doing further actions.

reactOption :: String -> String -> ContIO String
reactOption resp message = do 
    liftIO $ do
      putStr "enter "
      putStr resp
      putStr "\t to:"
      putStrLn message
    x <- react (setCallback resp) (return ())
    if  x /= resp then empty else do     -- if it is not the expected value, give up
          liftIO $ print resp
          return resp                 -- return it to his continuation
  

-- the thread that execute the callbacks in a loop
consoleLoop = do
    x   <- getLine 
    mbs <- readIORef rcb
    -- for each string entered, execute all the callbacks
    mapM (\cb -> forkIOE $ cb x )  $ M.elems mbs
    consoleLoop
  `catch` \(e:: SomeException) -> print "EXCP"

    



testAlternative=  do 
    r <- async (return "hello") <|> async (return "world") <|> async (return "world2")
    liftIO $ print r

-- > main= keep testAlternative
-- "hello"
-- "world"
-- "world2"

mainReact = do
    fork consoleLoop
    r <- (reactOption "hello" "hello")  <|>  (reactOption "world" "world")
    liftIO $ putStr "received: " >> print r
    where
    fork f= (async f >> empty) <|> return()

-- >main= keep mainReact
-- enter hello      to:hello
-- enter world      to:world
-- > hello
-- received: "hello"
-- > world
-- received: "world"
-- > hello
-- received: "hello" 
    
combination=  do
    r <-  ( async (threadDelay 10000 >> return "hello ")  <>  return "world")  <|> return "world2"
    liftIO $ putStrLn  r
       
-- > main= keep combination
-- world2
-- hello world

  
  


















looptest= runCont $ do
    setState "hello"
    r <- liftIO $ newIORef 0
    sum  r 1000000
    s <-   getState 
    liftIO $ putStrLn s
  where
  sum r 0= do r <- liftIO $ readIORef r; liftIO $ print r
  sum r x= do
     liftIO $ modifyIORef r $ \v -> v + x 
     sum r $x -1 




-- option using STM and waitEvents
inputLoop :: IO()
inputLoop= do
  l <- getLine 
  atomically (writeTVar mvline l)  
  inputLoop

wait = unsafePerformIO newEmptyMVar

mvline= unsafePerformIO $  newTVarIO ""

option :: String -> String -> ContIO  String
--option :: [Char] -> Cont r (StateT t IO) [Char]
option resp message= do
  liftIO $ do
    putStr "enter "
    putStr resp
    putStr "\t to:" 
    putStrLn message
  waitEvents . atomically $ do
              x <- readTVar mvline
              if  x== resp then  writeTVar mvline "" >> return resp else GHC.Conc.retry               
     
     -- callCC :: ((a -> Cont r StateIO b) -> Cont r m a) -> Cont r m a
options=do
  forkIOE $ inputLoop
  keep $ do 
    r <- option "hello" "hello" <|> option "world" "world" 
    liftIO $ putStr "received: " >> print r 
     
----------------------------------backtracking ------------------------

{- Exceptions pose another problem for composability specially in long running programs where an exception
shoould free resources or undo actions and then retunr to the normal flow. There may be a stack of actions and
resources that must be undone.

take for example this -}

emarket= productNavigation >>= reserve  >>= payment
  where
  reserve book = updateDB1 book >> updateDB2 book
  productNavigation= liftIO $ putStrLn "navigating the list of products" >> return "book"

{- If payment fails , unreservation involves two cancellations in each database. A mechanism using exceptions
would clutter the code with obscure complications in the 'reserve' code and forces this
routine to decide what to do next. conditional code in 'payment' would force this computation to know about
reservation details. All that is a problem for composability, modularity, maintainability, separation of
  concern etc.

Exceptions is a particular kind of backtracking. Let's define a form of backtracking among monadic statements
that is capable of doing some action and let the programmer to decide either executing further actions or
continue forward. In the previous example, imagine that 'payment' fail, but we have another card:
-}
data CardFailed= CardFailed  deriving Show
data CardThirdAttemptFailed = CardThirdAttemptFailed deriving Show
data Counter= Counter (IORef Int) deriving Typeable

instance Exception CardFailed
instance Exception CardThirdAttemptFailed

payment book= do
        setState newCounter
        pleaseEnterCard `onException'` (\(e :: CardFailed ) ->   do
                                                  Counter rn <- getState <|> return newCounter
                                                  n<- liftIO $ readIORef rn
                                                 
                                                  if n==2 then liftIO $ throw CardThirdAttemptFailed else do
                                                      liftIO $ writeIORef rn $ n+1
                                                      pleaseEnterCard
                                                      continue)
        pay 
        where
        pay= throw CardFailed
        newCounter=  Counter (unsafePerformIO $ newIORef 0)

        pleaseEnterCard = liftIO $ print "Please enter Card"

updateDB1 book=  update 1 book `onException'` \(e :: CardThirdAttemptFailed) -> unreserve 1 book

updateDB2 book=  update 2 book `onException'` \(e :: CardThirdAttemptFailed) -> unreserve 2 book

update n _= liftIO $ putStr "Updating database" >> print n 

unreserve n _= liftIO $  putStr "unreserving book in database" >> print n 

{-
We use a data structure that contains an stack of exception handlers and associated continuations.
We generalize the 'backtracking' data not only for exceptions but for any kind of data types. The first
field contains the data transported by the backtracking being carried out.
-}

data Backtrack b= forall a c. Backtrack{backtracking :: Maybe b
                                     ,backStack :: [(b ->ContIO  c,c -> ContIO  a)] }
                                     deriving Typeable



-- | Delete all the undo actions registered till now for the given track id.
-- backCut :: (Typeable b, Show b) => b -> ContIO ()
backCut reason=
     delData $ Backtrack (Just reason)  [] 

-- | 'backCut' for the default track; equivalent to @backCut ()@.
undoCut ::  ContIO ()
undoCut = backCut ()

-- | Run the action in the first parameter and register the second parameter as
-- the undo action. On undo ('back') the second parameter is called with the
-- undo track id as argument.
--
{-# NOINLINE onBack #-}
onBack :: (Typeable b, Show b) => ContIO a -> ( b -> ContIO a) -> ContIO a
onBack ac back =   registerBack  ac back
  
   where
         
   typeof :: (b -> ContIO a) -> b
   typeof = undefined

-- | 'onBack' for the default track; equivalent to @onBack ()@.
onUndo ::  ContIO a -> ContIO a -> ContIO a
onUndo x y= onBack x (\() -> y)



-- | Register an undo action to be executed when backtracking. The first
-- parameter is a "witness" whose data type is used to uniquely identify this
-- backtracking action. The value of the witness parameter is not used.
--
--{-# NOINLINE registerUndo #-}
-- registerBack :: (Typeable a, Show a) => (a -> ContIO a) -> a -> ContIO a
registerBack  ac back = callCC $ \k -> do
   md <- getData `asTypeOf` (Just <$> (backStateOf $ typeof back))  
   case md of
        Just (bss@(Backtrack b (bs@((back',_):_)))) ->
           setData $  Backtrack b  ( (back,  k):   unsafeCoerce bs)
        Just (Backtrack b []) -> setData $ Backtrack b  [(back , k)]
        Nothing ->  do
           setData $ Backtrack mwit  [  (back , k)] 
   ac
  
   where


   typeof :: (b -> ContIO a) -> b
   typeof = undefined
   mwit= Nothing `asTypeOf` (Just $ typeof back)

--
-- | For a given undo track id, stop executing more backtracking actions and
-- resume normal execution in the forward direction. Use it inside an undo
-- action.
--
forward :: (Typeable b, Show b) => b -> ContIO ()
forward reason=  do
    Backtrack _ stack <- getData `onNothing`  (backStateOf reason)
    setData $ Backtrack(Nothing `asTypeOf` Just reason)  stack


-- | Start the undo process for the given undo track id. Performs all the undo
-- actions registered till now in reverse order. An undo action can use
-- 'forward' to stop the undo process and resume forward execution. If there
-- are no more undo actions registered execution stops and a 'stop' action is
-- returned.
--
back :: (Typeable b, Show b) => b -> ContIO a
back reason = do
  Backtrack _ cs <- getData  `onNothing`  backStateOf  reason
  let  bs= Backtrack (Just reason) cs
  setData bs
  goBackt bs           
                                               
  where

  goBackt (Backtrack _ [] )= empty                     
  goBackt (Backtrack Nothing _ )= error "goback: no reason"

  goBackt (Backtrack (Just reason) ((handler,cont) : bs))= do

        x <- unsafeCoerce handler reason                                       

        Backtrack mreason _ <- getData `onNothing`  backStateOf  reason
                                                       
        case mreason of
                  Nothing    -> do 
                         unsafeCoerce $ cont x                        
                  justreason -> do
                        setData $ Backtrack justreason bs
                        goBackt $ Backtrack justreason bs             
                        empty

backStateOf :: (Monad m, Show a, Typeable a) => a -> m (Backtrack a)
backStateOf reason= return $ Backtrack (Nothing `asTypeOf` (Just reason)) []


{- Now we apply the general backtracking mechanism for exceptions 
first we manage an exception as data that will be backtracked with the above primitives
but also we need to catch every exception trown to be handled with that mechanism -}
------ exceptions ---
--
-- | Install an exception handler. Handlers are executed in reverse (i.e. last in, first out) order when such exception happens in the
-- continuation. Note that multiple handlers can be installed in sequence for the same exception type.
--
-- The semantic is thus very different than the one of `Control.Exception.Base.onException`
onException :: Exception e => (e -> ContIO  ()) -> ContIO ()
onException exc= return () `onException'` exc

-- | this variant allows  'doThis `onException'`  undoThis
onException' :: Exception e => ContIO a -> (e -> ContIO a) -> ContIO a
onException' mx f= onAnyException mx $ \e ->
    case fromException e of
       Nothing -> return $ error "do nothing,this should not be evaluated"
       Just e'  -> f e'
  where
  --onAnyException :: ContIO a -> (SomeException ->ContIO a) -> ContIO a
  onAnyException mx f= ioexp `onBack` f
    where 
    ioexp = callCC $ \cont -> do
       st <- get
       ioexp' $ runContState st (mx >>=cont ) `catch` exceptBack st
    
    ioexp' mx= do
      (mx,st') <- liftIO  mx
      put st'
      case mx of
        Nothing -> empty 
        Just x  -> return x


-- this is the code that catches any exception and call the backtracking mechanism
exceptBack st = \(e ::SomeException) -> do  -- recursive catch itself
                      if (isNothing (fromException e:: Maybe Empty))
                            then runContState st  (back e )  `catch` exceptBack st
                            else throw $ Empty st
               


  

-- | Delete all the exception handlers registered till now.
cutExceptions :: ContIO ()
cutExceptions= backCut  (undefined :: SomeException)

-- | Use it inside an exception handler. it stop executing any further exception
-- handlers and resume normal execution from this point on.
continue :: ContIO ()
continue = forward (undefined :: SomeException)

-- | catch an exception in a Cont block
--
-- The semantic of catcht is the same than `catch` but the computation and the exception handler 
-- can be multithreaded, reactive etc.
catcht :: Exception e => ContIO a -> (e -> ContIO a) -> ContIO a
catcht mx exc= do
      rpassed <- liftIO $ newIORef False
      sandbox  $ do
         delData $ Backtrack (Just (undefined :: SomeException))  [] 

         r <- onException'  mx $ \e -> do
                  passed <- liftIO $ readIORef rpassed
                  if not passed then unsafeCoerce continue >> exc e  else empty
         liftIO $ writeIORef rpassed True
         return r
         
   where
   sandbox :: ContIO a -> ContIO a
   sandbox  mx= do
     exState <- getData `onNothing` backStateOf (undefined :: SomeException)
     mx   <* setState exState 

-- | throw an exception in the Cont monad
throwt :: Exception e => e -> ContIO a
throwt= back . toException

{- Finally we need an extensible state management, Rich Hikey style, adapted to Haskell which may transport
data structures for composing effects, like the backtracking mechanism and the alternative mechanism but
also for any need of the application programmer. It is a type-indexed map with convenience accessors -}

-- * Extensible State: Session Data Management

-- |  If the data is found, a
-- 'Just' value is returned. Otherwise, a 'Nothing' value is returned.
getData :: (MonadState Stat m, Typeable a) => m (Maybe a)
getData = resp
  where resp = do
          list <- gets mfData
          case M.lookup (typeOf $ typeResp resp) list of
            Just x  -> return . Just $ unsafeCoerce x
            Nothing -> return Nothing
        typeResp :: m (Maybe x) -> x
        typeResp = undefined

-- | Retrieve a previously stored data item of the given data type from the
-- monad state. The data type to retrieve is implicitly determined from the
-- requested type.
-- If the data item is not found,  'empty' is returned.
-- Remember that an empty value stops the monad computation. If you want to
-- print an error message or a default value in that case, you can use an
-- 'Alternative' composition. For example:
--
-- > getSData <|> error "no data"
-- > getInt = getSData <|> return (0 :: Int)
getSData :: Typeable a => ContIO a
getSData =    do
        mx <- getData
        case mx of
          Nothing -> empty 
          Just x  -> return x

-- | Same as `getSData`
getState :: Typeable a => ContIO a
getState = getSData

-- | 'setData' stores a data item in the monad state which can be retrieved
-- later using 'getData' or 'getSData'. Stored data items are keyed by their
-- data type, and therefore only one item of a given type can be stored. A
-- newtype wrapper can be used to distinguish two data items of the same type.
--
-- @
-- import Control.Monad.IO.Class (liftIO)
-- import Cont.Base
-- import Data.Typeable
--
-- data Person = Person
--    { name :: String
--    , age :: Int
--    } deriving Typeable
--
-- main = keep $ do
--      setData $ Person "Alberto"  55
--      Person name age <- getSData
--      liftIO $ print (name, age)
-- @
setData :: (MonadState Stat m, Typeable a) => a -> m ()
setData x = modify $ \st -> st { mfData = M.insert t (unsafeCoerce x) (mfData st) }
  where t = typeOf x

-- | Accepts a function that takes the current value of the stored data type
-- and returns the modified value. If the function returns 'Nothing' the value
-- is deleted otherwise updated.
modifyData :: (MonadState Stat m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyData f = modify $ \st -> st { mfData = M.alter alterf t (mfData st) }
  where typeResp :: (Maybe a -> b) -> a
        typeResp   = undefined
        t          = typeOf (typeResp f)
        alterf mx  = unsafeCoerce $ f x'
          where x' = case mx of
                       Just x  -> Just $ unsafeCoerce x
                       Nothing -> Nothing

-- | Same as modifyData
modifyState :: (MonadState Stat m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyState = modifyData

-- | Same as 'setData'
setState :: (MonadState Stat m, Typeable a) => a -> m ()
setState = setData

-- | Delete the data item of the given type from the monad state.
delData :: (MonadState Stat m, Typeable a) => a -> m ()
delData x = modify $ \st -> st { mfData = M.delete (typeOf x) (mfData st) }

-- | Same as 'delData'
delState :: (MonadState Stat m, Typeable a) => a -> m ()
delState = delData


-- STRefs for the Cont monad


-- | If the first parameter is 'Nothing' return the second parameter otherwise
-- return the first parameter..
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'


testBack = do

   runCont $ do
        return () 
        r <-  async (print "hello")  `onBack` \s ->  liftIO $ print $ "received: 111"++ s 
        r <-  async (print "world")  `onBack` \s ->  liftIO $ print $ "received: 222"++ s 

        back "exception"
        empty
   takeMVar wait


testException= do
   runCont $ do
        onException $ \(s :: SomeException) -> liftIO $  print $ "received: 111"++ show s 
        async $ print "$$$$$$$$$$$$"
        -- r <-  async (print "hello")  `onException'` \(s :: SomeException) -> liftIO $  print $ "received: 111"++ show s 
        -- r <-  async (print "world")  `onException'` \(s :: SomeException) -> liftIO $  print $ "received: 222"++ show s 
        liftIO $ print "AFTER"
        liftIO $ myThreadId >>= print

        error "exception"
   takeMVar wait

mainCatch= do
   runCont $ do
        async $ print "hello"
        error "error" 
        return ()
      `catcht` (\(e :: SomeException) -> liftIO $ print $ "RECEIVED " ++ show e)
      
   takeMVar wait


amounts= do
    liftIO $ putStrLn "this example read quantities from a list every five seconds let the user to increase or"
    liftIO $ putStrLn "decrease the price. whenever one of the two factors change, the amount is printed"
    amount <- price * quantity
    liftIO $ do putStr "new amount: "; print amount
    where
    quantity= choose' [10..100::Int]
         where
         choose' []= empty
         choose' (x:xs)= do 
             liftIO $ threadDelay 5000000 
             async (return x) <|> choose' xs

    rprice = unsafePerformIO $ newIORef 10
    
    price= (do reactOption "inc" "increase";modify 1)    <|> 
           (do reactOption "dec" "decrease";modify (-1))
 
    modify inc= liftIO $ do
      price <- readIORef rprice 
      writeIORef rprice $ price + inc 
      return price
 

-- Annex: all the options composed in a single main program.

main= keep $ do 
   fork consoleLoop
   examples
   where
   fork f= (async f >> empty) <|> return()
  
examples= do
      (reactOption "menu" "show the menu" >> return()) <|> return ()
      combination' <|> testAlternative'   <|> chooseTwo' <|> pythagoras' 
                   <|> emarket' <|> amounts'
     
      where
      amounts'= do 
        reactOption "amt"  "amounts: steaming/threading/reactive"
        amounts

      testAlternative'= do 
         reactOption "alt" "alternative parallel example"
         testAlternative
      
      combination'= do 
         reactOption "comb" "parallel combination of alternative and monoid"
         combination

      chooseTwo'= do
         reactOption "two" "parallel list processing"
         choosetwo 
      
      pythagoras'= do
         reactOption "pyt"  "pythagoras triangle"
         pythagoras

      emarket'= do 
         reactOption "mkt"  "emarket: example of backtracking and exceptions"
         emarket
      