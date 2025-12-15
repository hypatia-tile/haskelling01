module MyLib (mainLoop) where

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
