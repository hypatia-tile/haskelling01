module Repl.Core where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State.Strict (StateT (..), runStateT)
import Repl.Arithmetic (arithAppShell)
import Repl.Command
import Repl.State

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

regularAppShell :: String -> IO ()
regularAppShell input = do
  putStrLn "Executing command..."
  putStrLn $ "You entered: " ++ input
  putStrLn "Continuing..."
