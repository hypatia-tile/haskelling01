module Main where

import MyLib (mainLoop)

main :: IO ()
main = do
  putStrLn "Hello, This is Tiny REPL!"
  mainLoop
