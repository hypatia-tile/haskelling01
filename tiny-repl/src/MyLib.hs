module MyLib (AppState (..), mainLoop) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict

mainLoop :: AppState -> IO ()
mainLoop initialState = do
  putStrLn "Input something (or type 'exit' to quit):"
  input <- getLine
  result <- runExceptT $ runStateT (interpret input) initialState
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      mainLoop initialState
    Right ((), newState) -> case newState of
      ExitAppState -> putStrLn "Exiting..."
      _ -> do
        putStrLn "History:"
        forM_ (history newState) putStrLn
        putStrLn "Executing command..."
        print =<< execute newState
        putStrLn "Continuing..."
        mainLoop newState

interpret :: String -> AppM ()
interpret input = StateT $ \state' ->
  case appCommand input state' of
    Left err -> throwError err
    Right newState -> return ((), newState)

type AppM = StateT AppState (ExceptT AppError IO)

data AppState
  = RegularAppState
      { history :: [String]
      , execute :: IO String
      }
  | ExitAppState

appCommand :: String -> AppState -> Either AppError AppState
appCommand cmd state'
  | null cmd = Left (ParseError "Empty command")
  | otherwise = case cmd of
      "exit" -> Right ExitAppState
      "hello" -> Right $ RegularAppState (cmd : history state') (return "world")
      "add" -> Right $ RegularAppState (cmd : history state') (return "added")
      "sub" -> Right $ RegularAppState (cmd : history state') (return "subtracted")
      _ -> Left (UnknownCommand cmd)

data AppError
  = ParseError String
  | UnknownCommand String
  | DomainError String
  deriving (Show)
