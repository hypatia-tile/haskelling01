-- Now that, I understand fundamental in StateT
--  except some advansed usage like lifting inner monad UnsupportedOperation
-- In this example, I will try to create 'return' function for StateT
import Control.Monad.State.Strict

return' :: (Monad m) => a -> StateT s m a
return' val = StateT $ \s -> return (val, s)

runStateT' :: (Monad m) => StateT s m a -> s -> m (a, s)
runStateT' (StateT f) = f

main = do
  let (st :: StateT String IO Int) = return' 1
  result <- runStateT' st "Hello"
  print result
  runStateT' st "Hello" -- Same thing to above two lines
