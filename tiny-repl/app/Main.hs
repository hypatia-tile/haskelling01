module Main where

import MyLib (AppMode (..), AppState (..), mainLoop)

main :: IO ()
main = do
  let
    appState :: AppState
    appState = AppState{commandHistory = [], appStateMode = RegularMode}
  mainLoop appState
