module Repl.State where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT)

data AppState
  = AppState
  { commandHistory :: [String]
  , appStateMode :: AppMode
  }

data AppMode
  = RegularMode
  | ArithMode
  | ExitMode

data AppError
  = ParseError String
  | UnknownCommand String
  | DomainError String
  deriving (Show)

type AppM = StateT AppState (ExceptT AppError IO)

newappState :: AppState
newappState = AppState [] RegularMode

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
