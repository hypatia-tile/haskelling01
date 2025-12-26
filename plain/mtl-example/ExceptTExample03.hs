-- rewrite the ExceptT's instantiation of MonadError
import Control.Monad.Except hiding (ExceptT (..), runExceptT)

{- | Define ExceptT from scratch
Intuitive:
  An ExceptT is a wrapper around a computation that can either fail with an error of type 'e'
  or succeed with a value of type 'a', all within a monadic context 'm'.
-}
newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

instance (Functor m) => Functor (ExceptT e m) where
  -- ExceptT $ (fmap :: c -> d -> m c -> m d) (fmap f :: Either a -> Either b) mea
  -- ExceptT $ (x :: m (Either e a) -> m (Either e b)) (mea :: m (Either e a))
  -- ExceptT $ (x mea :: m (Either e b))
  fmap f (ExceptT mea) = ExceptT $ fmap (fmap f) mea

instance (Functor m, Monad m) => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right

  -- mf :: m (Either e (a -> b))
  -- ma :: m (Either e a)
  -- <*> ::  ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
  ExceptT mf <*> ExceptT ma = ExceptT $ do
    ef <- mf
    ea <- ma
    return (ef <*> ea)

instance (Monad m) => Monad (ExceptT e m) where
  return = pure

  -- ma :: ExceptT e m a
  -- f :: a -> ExceptT e m b
  -- >>= :: ExceptT e m a -> (a -> ExceptT e m b)
  ExceptT ma >>= f = ExceptT $ do
    ea <- ma
    case ea of
      Left e -> return (Left e)
      Right a -> runExceptT (f a)

main :: IO ()
main = do
  let
    computation :: ExceptT String IO Int
    computation = do
      x <- ExceptT $ return (Right 10)
      if x > 5
        then ExceptT $ return $ Left "Value too large"
        else return (x * 2)
  result <- runExceptT computation
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right val -> putStrLn $ "Success: " ++ show val
