module Main where

import MyLib (AppState (..), mainLoop)

main :: IO ()
main = do
  let
    appState :: AppState
    appState = RegularAppState [] (return "init")
  mainLoop appState
