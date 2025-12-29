module Repl.Command where

import Data.List (find)
import Repl.State

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
