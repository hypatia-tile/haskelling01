module Repl.Core (myApp) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State.Strict (StateT (..), runStateT)
import Repl.Arithmetic (arithAppShell)
import Repl.Command (AppShellCommand, appCommand)
import Repl.State

myApp :: IO ()
myApp = mainLoop (MyReplApp newappState)

class MyRepl a where
  mainLoop :: a -> IO ()

newtype MyReplApp s = MyReplApp {runMyReplApp :: s}
  deriving (Show)

instance (AppShellState s, AppShellCommand s) => MyRepl (MyReplApp s) where
  mainLoop currentState = do
    putStrLn "Input something (or type 'exit' to quit):"
    input <- getLine
    let currentState' = runMyReplApp currentState
    result <- runExceptT $ runStateT (interpret input) currentState'
    case result of
      Left err -> do
        putStrLn $ "Error: " ++ show err
        mainLoop (MyReplApp currentState')
      Right ((), newState) -> do
        putStrLn "History so far: "
        showHist newState
        case runMode newState of
          ExitMode -> putStrLn "Exiting..."
          mode -> do
            case mode of
              ArithMode -> arithAppShell
              RegularMode -> regularAppShell input
            mainLoop (MyReplApp newState)
   where
    interpret :: (AppShellState s, AppShellCommand s) => String -> AppM s ()
    interpret input = StateT $ \state' ->
      case appCommand input state' of
        Left err -> throwError err
        Right newState -> return ((), newState)
    regularAppShell :: String -> IO ()
    regularAppShell input = do
      putStrLn "Executing command..."
      putStrLn $ "You entered: " ++ input
      putStrLn "Continuing..."
