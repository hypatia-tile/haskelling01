module MyLib (mainLoop) where

-- import Control.Monad.Except
-- import Control.Monad.State

mainLoop :: IO ()
mainLoop = do
  putStrLn "Input something (or type 'exit' to quit):"
  input <- getLine
  case interpret input of
    Left err -> do
      putStrLn $ "Error: " ++ show err
    Right appState -> do
      putStrLn $ output appState
      if exit appState
        then putStrLn "Exiting..."
        else mainLoop

-- TODO: Implement a real interpreter
interpret :: String -> Either AppError AppState
interpret input = case input of
  "exit" -> Right $ AppState True "Goodbye!"
  _ -> Right $ AppState False ("Interpreted: " ++ input)

-- TODO: Define a proper AppState and AppError
data AppState = AppState
  { exit :: Bool
  , output :: String
  }

data AppError
  = ParseError String
  | UnknownCommand String
  | DomayinError String
  deriving (Show)
