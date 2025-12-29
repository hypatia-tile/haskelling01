module MyLib (AppMode (..), AppState (..), myApp) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict
import Data.List (find)
import Repl.Arithmetic

myApp :: IO ()
myApp = mainLoop newappState
 where
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
        showHist newState
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
 where
  appCommand :: String -> AppState -> Either AppError AppState
  appCommand cmd state'
    | null cmd = Left (ParseError "Empty command")
    | otherwise = do
        command <- parseAppCommand cmd
        return $ state'{appStateMode = cmdMode command, commandHistory = cmd : commandHistory state'}

type AppM = StateT AppState (ExceptT AppError IO)

data AppState
  = AppState
  { commandHistory :: [String]
  , appStateMode :: AppMode
  }

newappState :: AppState
newappState = AppState [] RegularMode

data AppMode
  = RegularMode
  | ArithMode
  | ExitMode

data AppCommand = AppCommand
  { cmdName :: String
  , cmdMode :: AppMode
  }

class AppShellState a where
  showHist :: a -> IO ()

instance AppShellState AppState where
  showHist appState = do
    let
      histories :: [(Int, String)]
      histories = zip [1 ..] (reverse . commandHistory $ appState)
    putStrLn "Command History: "
    forM_ histories $ \(i, s) ->
      putStrLn $ show i ++ ": " ++ s

exitCommand, helloCommand, arithCommand :: AppCommand
exitCommand = AppCommand "exit" ExitMode
helloCommand = AppCommand "hello" RegularMode
arithCommand = AppCommand "arith" ArithMode

allCommands :: [AppCommand]
allCommands = [exitCommand, helloCommand, arithCommand]

parseAppCommand :: String -> Either AppError AppCommand
parseAppCommand input =
  case find (\cmd -> cmdName cmd == input) allCommands of
    Just cmd -> Right cmd
    Nothing -> Left (UnknownCommand $ "Unknown command: " ++ input)

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
