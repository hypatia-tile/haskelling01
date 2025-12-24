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
  g >>= f = StateT $ \s -> do
    (a, newState) <- runStateT g s
    runStateT (f a) newState

stateCompute :: StateT Int IO String
stateCompute = do
  StateT $ \s -> do
    putStrLn $ "CurrentState: " ++ show s
    return ("Value is " ++ show s, s + 1)

main :: IO ()
main = do
  let
    computation :: StateT Int IO String
    computation = do
      stateCompute
      stateCompute
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
