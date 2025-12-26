module MyLib (mainLoop) where

import Control.Monad.Except

-- import Control.Monad.Except
-- import Control.Monad.State

mainLoop :: IO ()
mainLoop = do
  putStrLn "Input something (or type 'exit' to quit):"
  input <- getLine
  result <- runExceptT (interpret input)
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      mainLoop
    Right appState -> case appState of
      ExitAppState -> putStrLn "Exiting..."
      RegularAppState output' -> do
        putStrLn $ "Output: " ++ output'
        mainLoop

-- TODO: Implement a real interpreter
interpret :: String -> ExceptT AppError IO AppState
interpret input = case input of
  "exit" -> ExceptT $ return (Right ExitAppState)
  "hello" -> ExceptT $ return (Right (RegularAppState "Hello, World!"))
  _ -> throwError (UnknownCommand input)

-- TODO: Define a proper AppState and AppError
data AppState
  = RegularAppState
      { output :: String
      }
  | ExitAppState

data AppError
  = ParseError String
  | UnknownCommand String
  | DomayinError String
  deriving (Show)
