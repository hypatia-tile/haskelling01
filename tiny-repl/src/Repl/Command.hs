module Repl.Command (
  exitCommand,
  helloCommand,
  arithCommand,
  parseAppCommand,
  appCommand,
  AppCommand (cmdMode),
) where

import Data.List (find)
import Repl.State (AppError (..), AppMode (..), AppState (..))

class AppShellCommand a where
  appCommand :: String -> a -> Either AppError a

instance AppShellCommand AppState where
  appCommand cmd state'
    | null cmd = Left (ParseError "Empty command")
    | otherwise = do
        command <- parseAppCommand cmd
        return $ state'{appStateMode = cmdMode command, commandHistory = cmd : commandHistory state'}

data AppCommand = AppCommand
  { cmdName :: String
  , cmdMode :: AppMode
  }

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
