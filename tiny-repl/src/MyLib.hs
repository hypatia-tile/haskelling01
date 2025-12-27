module MyLib (AppState (..), mainLoop) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict

mainLoop :: AppState -> IO ()
mainLoop initialState = do
  putStrLn "Input something (or type 'exit' to quit):"
  input <- getLine
  result <- runExceptT $ runStateT (interpret input) initialState
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      mainLoop initialState
    Right ((), newState) -> case newState of
      ExitAppState -> putStrLn "Exiting..."
      ArithAppState -> do
        putStrLn "Entering arithmetic shell..."
        arithResult <- runExceptT $ evalStateT arithShell []
        case arithResult of
          Left err -> putStrLn $ "Arithmetic shell error: " ++ err
          Right _ -> putStrLn "Exiting arithmetic shell."
        mainLoop newState
      _ -> do
        putStrLn "History:"
        forM_ (history newState) putStrLn
        putStrLn "Executing command..."
        print =<< execute newState
        putStrLn "Continuing..."
        mainLoop newState

interpret :: String -> AppM ()
interpret input = StateT $ \state' ->
  case appCommand input state' of
    Left err -> throwError err
    Right newState -> return ((), newState)

type AppM = StateT AppState (ExceptT AppError IO)

data ArithExpr
  = Push Int
  | Add
  | Sub
  deriving (Show)

data AppState
  = RegularAppState
      { history :: [String]
      , execute :: IO String
      }
  | ArithAppState
  | ExitAppState

evalArith :: [ArithExpr] -> Either String Int
evalArith exprs = case foldr (flip go) [] exprs of
  [Push n] -> Right n
  xs -> Left $ "Stack did not end with a single value\n remaining stack: " ++ show xs
 where
  go :: [ArithExpr] -> ArithExpr -> [ArithExpr]
  go (Push x : Add : rest) (Push y) = Push (x + y) : rest
  go (Push x : Sub : rest) (Push y) = Push (y - x) : rest
  go stack x = x : stack

arithShell :: StateT [ArithExpr] (ExceptT String IO) ()
arithShell = do
  exprs' <- arithInputLoop []
  liftIO $ putStrLn $ "Final expressions: " ++ show exprs'
  return ()
 where
  arithInputLoop :: [ArithExpr] -> StateT [ArithExpr] (ExceptT String IO) Int
  arithInputLoop exprs = do
    liftIO $ putStrLn $ "Current expressions: " ++ show (reverse exprs)
    liftIO $ putStrLn "Arith> "
    input <- liftIO getLine
    case words input of
      ["push", nStr] -> case reads nStr of
        [(n, "")] -> arithInputLoop (Push n : exprs)
        _ -> do
          liftIO $ putStrLn "Invalid number"
          arithInputLoop exprs
      ["add"] -> arithInputLoop (Add : exprs)
      ["sub"] -> arithInputLoop (Sub : exprs)
      ["eval"] -> do
        case evalArith exprs of
          Left err -> do
            throwError err
          Right val -> return val
      _ -> do
        throwError "Unknown command"

data AppCommand
  = Exit
  | Hello
  | Arith

parseAppCommand :: String -> Either AppError AppCommand
parseAppCommand cmd
  | cmd == "exit" = Right Exit
  | cmd == "hello" = Right Hello
  | cmd == "arith" = Right Arith
  | otherwise = Left (UnknownCommand $ "Unknown command: " ++ cmd)

appCommand :: String -> AppState -> Either AppError AppState
appCommand cmd state'
  | null cmd = Left (ParseError "Empty command")
  | otherwise = case parseAppCommand cmd of
      Right Exit -> Right ExitAppState
      Right Hello ->
        Right $
          RegularAppState
            { history = history state' ++ [cmd]
            , execute = return "Hello, World!"
            }
      Right Arith -> Right ArithAppState
      Left err -> Left err

data AppError
  = ParseError String
  | UnknownCommand String
  | DomainError String
  deriving (Show)
