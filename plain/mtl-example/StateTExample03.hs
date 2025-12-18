-- Now that I understand the foundation of StateT, except some advanced usage like lifting.
-- In this example, I will try to create 'return' and 'runStateT' functions for StateT
import Control.Monad.State.Strict

return' :: (Monad m) => a -> StateT s m a
return' val = StateT $ \s -> return (val, s)

runStateT' :: (Monad m) => StateT s m a -> s -> m (a, s)
runStateT' (StateT f) = f

{- | Lifts a computation from the inner monad 'm' to the StateT monadTransformer.
  Intuitive:
    contract that we use the value extracted from the context of inner monad 'm'
    wrap it with the current state 's' without modifying it.
-}
liftToStateT :: (Monad m) => m a -> StateT s m a
liftToStateT ma = StateT $ \s -> do
  a <- ma
  return (a, s)

safeDivide :: Int -> Int -> Either String Int
safeDivide _ 0 = Left "Division by zero"
safeDivide x y = Right (x `div` y)

liftedState :: StateT String IO ()
liftedState = do
  v <- get
  lift $ print v

main = do
  let (st :: StateT String IO Int) = return' 1
  print =<< runStateT' st "Hello"
  runStateT' liftedState "Lifted State"
