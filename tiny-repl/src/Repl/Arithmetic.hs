module Repl.Arithmetic where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.State.Strict

data ArithExpr
  = ANum Int
  | Add
  | Sub
  deriving (Show)

arithAppShell :: IO ()
arithAppShell = do
  putStrLn "Entering arithmetic shell..."
  arithResult <- runExceptT arithEvalInput
  case arithResult of
    Left err -> putStrLn $ "Arithmetic shell error: " ++ err
    Right result -> do
      putStrLn $ "Result: " ++ show result
      putStrLn "Exiting arithmetic shell."

evalArith :: [ArithExpr] -> Either String Int
evalArith exprs = case foldr (flip applyExp) [] exprs of
  [ANum n] -> Right n
  xs -> Left $ "Stack did not end with a single value\n remaining stack: " ++ show xs
 where
  applyExp :: [ArithExpr] -> ArithExpr -> [ArithExpr]
  applyExp (ANum x : Add : rest) (ANum y) = applyExp rest $ ANum (x + y)
  applyExp (ANum x : Sub : rest) (ANum y) = applyExp rest $ ANum (x - y)
  applyExp stack x = x : stack

arithEvalInput :: ExceptT String IO Int
arithEvalInput = arithInputLoop []
 where
  arithInputLoop :: [ArithExpr] -> ExceptT String IO Int
  arithInputLoop exprs = do
    liftIO $ putStrLn $ "Current expressions: " ++ show (reverse exprs)
    liftIO $ putStrLn "Arith> "
    input <- liftIO getLine
    case words input of
      ["push", nStr] -> case reads nStr of
        [(n, "")] -> arithInputLoop (ANum n : exprs)
        _ -> do
          liftIO $ putStrLn "Invalid number"
          arithInputLoop exprs
      ["add"] -> arithInputLoop (Add : exprs)
      ["sub"] -> arithInputLoop (Sub : exprs)
      ["eval"] -> case evalArith exprs of
        Left err -> throwError err
        Right val -> return val
      _ -> throwError "Unknown command"
