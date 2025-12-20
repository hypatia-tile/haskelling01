-- More interesting example
import Control.Monad.Trans.State

{- | We consider a stateful computation that performs safe division.
      The quotient is a new state, and we also return a log message.
      I try to use StateT with Either as the inner monad in following way.
-}
safeDiv :: Int -> Int -> Either String (String, Int)
safeDiv _ 0 = Left "Division by zero"
safeDiv x y = Right (show x ++ " divided by " ++ show y ++ " is " ++ show (x `div` y), x `div` y)

{- | create a stateful computation that performs safe division
      The state is the dividend, and the divisor is given as an argument.
-}
safeDivState :: Int -> StateT Int (Either String) String
safeDivState y = StateT $ safeDiv y

{- | Change the context of the stateful computation from Either to IO
    Intuitive:
      The safeDivState computation has a context of Either String,
      which means it can fail with an error message.
      We want to change this context to IO, so that we can perform
      side effect like logging.
-}
mapSafeDiv :: Either String (String, Int) -> IO ((), Int)
mapSafeDiv x = do
  putStrLn "Division started"
  res <- executeDivision x
  putStrLn "Division ended"
  return ((), res)
 where
  executeDivision :: Either String (String, Int) -> IO Int
  executeDivision c = case c of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return 0
    Right resp -> do
      putStrLn $ "Success: " ++ fst resp
      return (snd resp)

{- | A stateful computation that perform safe division with logging.
      Switch the context from Either to IO using mapStateT.
-}
loggedSafeDiv :: StateT Int IO ()
loggedSafeDiv = mapStateT mapSafeDiv $ safeDivState 10

main :: IO ()
main = do
  r1 <- runStateT loggedSafeDiv 10
  r2 <- runStateT loggedSafeDiv 3
  print r1
  print r2
  r2 <- runStateT loggedSafeDiv 0
  print "test"
  print r2
