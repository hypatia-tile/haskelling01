import Control.Monad.State.Strict

myFunc :: Int -> String
myFunc x = "The value is: " ++ show x

myState :: StateT Int IO String
myState = gets myFunc

evalMyState :: Int -> IO String
evalMyState = evalStateT myState

main :: IO ()
main = do
  result <- evalMyState 42
  putStrLn result
