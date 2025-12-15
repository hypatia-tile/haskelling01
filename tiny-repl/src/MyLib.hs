module MyLib (mainLoop) where

import Control.Monad.Except
import Control.Monad.State

mainLoop :: IO ()
mainLoop = do
  putStrLn "Input something (or type 'exit' to quit):"
  input <- getLine
  case interpret input of
    Just output -> do
      putStrLn output
      mainLoop
    Nothing -> putStrLn "Goodbye!"

interpret :: String -> Maybe String
interpret input = case input of
  "exit" -> Nothing
  _ -> Just $ "Interpreted: " ++ input

data AppState = AppState
  {}

data AppError
  = ParseError String
  | UnknownCommand String
  | DomayinError String
  deriving (Show)

type AppM = StateT AppState (ExceptT AppError IO)
