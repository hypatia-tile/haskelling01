module Repl.Arithmetic where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.State.Strict

data ArithExpr
  = Push Int
  | Add
  | Sub
  deriving (Show)

arithAppShell :: IO ()
arithAppShell = do
  putStrLn "Entering arithmetic shell..."
  arithResult <- runExceptT $ evalStateT arithShell []
  case arithResult of
    Left err -> putStrLn $ "Arithmetic shell error: " ++ err
    Right _ -> putStrLn "Exiting arithmetic shell."

evalArith :: [ArithExpr] -> Either String Int
evalArith exprs = case foldr (flip go) [] exprs of
  [Push n] -> Right n
  xs -> Left $ "Stack did not end with a single value\n remaining stack: " ++ show xs
 where
  go :: [ArithExpr] -> ArithExpr -> [ArithExpr]
  go (Push x : Add : rest) (Push y) = go rest $ Push (x + y)
  go (Push x : Sub : rest) (Push y) = go rest $ Push (x - y)
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
