module MyLib (AppMode (..), AppState (..), mainLoop) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict
import Repl.Arithmetic

mainLoop :: AppState -> IO ()
mainLoop initialState = do
  putStrLn "Input something (or type 'exit' to quit):"
  input <- getLine
  result <- runExceptT $ runStateT (interpret input) initialState
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      mainLoop initialState
    Right ((), newState) -> do
      putStrLn "History so far: "
      forM_ (commandHistory newState) putStrLn
      case appStateMode newState of
        ExitMode -> putStrLn "Exiting..."
        mode -> do
          case mode of
            ArithMode -> arithAppShell
            RegularMode -> regularAppShell input
          mainLoop newState

interpret :: String -> AppM ()
interpret input = StateT $ \state' ->
  case appCommand input state' of
    Left err -> throwError err
    Right newState -> return ((), newState)

type AppM = StateT AppState (ExceptT AppError IO)

data AppState
  = AppState
  { commandHistory :: [String]
  , appStateMode :: AppMode
  }

data AppMode
  = RegularMode
  | ArithMode
  | ExitMode

data AppCommand
  = Exit
  | Hello
  | Arith

parseAppCommand :: String -> Either AppError AppCommand
parseAppCommand cmd
  | cmd == "exit" = Right Exit
  | cmd == "hello" = Right Hello
  | cmd == "arith" = Right Arith
  | otherwise = Left (UnknownCommand $ "Unknown command: " ++ cmd)

appCommand :: String -> AppState -> Either AppError AppState
appCommand cmd state'
  | null cmd = Left (ParseError "Empty command")
  | otherwise = case parseAppCommand cmd of
      Right Exit -> Right $ state'{appStateMode = ExitMode}
      Right Hello ->
        Right $ state'{appStateMode = RegularMode}
      Right Arith -> Right $ state'{appStateMode = ArithMode}
      Left err -> Left err

regularAppShell :: String -> IO ()
regularAppShell input = do
  putStrLn "Executing command..."
  putStrLn $ "You entered: " ++ input
  putStrLn "Continuing..."

data AppError
  = ParseError String
  | UnknownCommand String
  | DomainError String
  deriving (Show)
