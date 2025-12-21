import Control.Monad (forM_)
import Control.Monad.State (
  MonadState (get),
  MonadTrans (lift),
  StateT,
  execStateT,
  modify,
 )

{- | Stateful computation
   func: function to update the state
   i: input valude to apply the function
-}
statefulCalc :: (Int -> Int -> Int) -> Int -> StateT Int IO ()
statefulCalc func i = do
  modify (func i)
  v <- get
  lift $ putStrLn $ "Compute " ++ show i ++ " ==> " ++ show v

sum' :: [Int] -> IO Int
sum' xs = (`execStateT` 0) $ do
  -- forM_ :: [Int] -> (Int -> StateT Int IO ()) -> StateT Int IO ()
  forM_ xs $ statefulCalc (+)

mapM_' :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_' action = foldr helper (return ())
 where
  helper x acc = action x >> acc

forM_' :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_' = flip mapM_'

sequenceA_' :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_' = foldr helper (pure ())
 where
  helper m k = m *> k
