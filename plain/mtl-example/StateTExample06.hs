-- In this example, I will rewrite the StateT's instantiations for important type classes.
import Control.Monad.Trans.State.Lazy hiding (StateT (..))

{- | Define the StateT type
| StateT is essentailly a function that takes a state of type 's'
  and returns monadic value 'm' containing a tuple of a result of type 'a' and the new statte 's'.
-}
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

{- | To instantiate StateT as a Functor
     I need to utilize lazy '~' pattern matching to avoid strict evaluation
     'f' is the functioon from 's' to 'm (a, s)'
-}
instance (Functor m) => Functor (StateT s m) where
  fmap f g = StateT $ \x -> fmap (\ ~(a, s) -> (f a, s)) (runStateT g x)

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT mf <*> StateT mx = StateT $ \s -> do
    ~(f, s') <- mf s
    ~(x, s'') <- mx s'
    return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  -- g :: StateT s m a
  -- f :: a -> StateT s m b
  -- TODO: Why this deinition work naturally?
  -- The Monad's law may following from the fact that m is also a Monad
  g >>= f = StateT $ \s -> do
    -- runStateT g s :: m (a, s)
    (a, newState) <- runStateT g s
    -- f :: a -> StateT s m b
    -- a :: a
    -- f a :: StateT s m b
    -- runStateT (f a) :: s -> m (b, s)
    -- runStateT (f a) newState :: m (b, s)
    runStateT (f a) newState

{- | 'statefulCounter' accepts a value of type 'Int' as state and:
        (1) Announce the current sate and return the value represented as a 'String'
        (2) Increment the state by 1
        These operation are performed within the 'IO' context.
-}
statefulCounter :: StateT Int IO String
statefulCounter = do
  StateT $ \s -> do
    putStrLn $ "CurrentState: " ++ show s
    return ("Value is " ++ show s, s + 1)

main :: IO ()
main = do
  let
    computation :: StateT Int IO String
    computation = do
      statefulCounter
      statefulCounter
  result <- runStateT computation 1
  putStrLn $ "Final Result: " ++ show result

main01 :: IO ()
main01 = do
  let
    computation = do
      modify (+ 1)
      modify (+ 1)
  result <- execStateT computation 1
  putStrLn $ "Final State: " ++ show result
