-- Now that I understand the foundation of StateT, except some advanced usage like lifting.
-- In this example, I will try to create 'return' and 'runStateT' functions for StateT
import Control.Monad.State.Strict

return' :: (Monad m) => a -> StateT s m a
return' val = StateT $ \s -> return (val, s)

runStateT' :: (Monad m) => StateT s m a -> s -> m (a, s)
runStateT' (StateT f) = f

main = do
  let (st :: StateT String IO Int) = return' 1
  print =<< runStateT' st "Hello"
