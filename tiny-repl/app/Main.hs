module Main where

main :: IO ()
main = do
  putStrLn "Hello, This is Tiny REPL!"
  putStrLn "Input something:"
  input <- getLine
  putStrLn $ "Your input was: " ++ input
