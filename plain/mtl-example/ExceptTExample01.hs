-- In this example, I will demonstrate the use of the ExceptT monad transformer from the mtl library.

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Char (isDigit, isLower, isUpper)
import Data.Either (either)

{- | ExceptT example that always results in an error.
      ExceptT is essentially a wrapper around Either that allows for computations that can fail with an error message.
-}
exceptExample :: ExceptT String IO ()
exceptExample = ExceptT $ return (Left "An Error Occurred")

main01 :: IO ()
main01 = do
  result <- runExceptT exceptExample
  case result of
    Left errMsg -> putStrLn $ "Computation failed with error: " ++ errMsg
    Right _ -> putStrLn "Computation succeeded"

{- | Revisit the Either Monad:
      It represents a value taht is either correct or error.
    The either functino in Data.Either module can handle Either value and applies the appropriate function based on whether the value is Left or Right.
      either :: (a -> c) -> (b -> c) -> Eitehr a b -> c
-}
eitherMul2 :: Either String Int -> Int
eitherMul2 = either (\_ -> -1) (* 2)

main02 :: IO ()
main02 = do
  print $ eitherMul2 $ Left "Error"
  print $ eitherMul2 $ Right 10

eitherTest1 :: Either () Int
eitherTest1 = do
  a <- Right 1
  Left ()
  return a

eitherTest2 :: Either () Int
eitherTest2 = do
  a <- Right 1
  Right 2
  return a

main03 :: IO ()
main03 = do
  print eitherTest1
  print eitherTest2

getch :: String -> Int -> Either String Char
getch s n
  | n < length s = Right (s !! n)
  | otherwise = Left $ "Index " ++ show n ++ " out of bounds for string of length " ++ show (length s)

testGetch :: String -> Either String String
testGetch s = do
  ch0 <- getch s 0
  ch1 <- getch s 1
  ch2 <- getch s 2
  unless (isUpper ch0) $ Left "First character is not uppercase"
  unless (isLower ch1) $ Left "Second charactor is not lowercase"
  unless (isDigit ch2) $ Left "Third character is not a digit"
  return [ch0, ch1, ch2]

main04 :: IO ()
main04 = do
  print $ testGetch "Aa0"
  print $ testGetch "A"
  print $ testGetch "aa0"
  print $ testGetch "Aaa"

testGetch2 :: String -> ExceptT String IO String
testGetch2 s = ExceptT $ return $ testGetch s

exceptExample2 :: ExceptT String IO ()
exceptExample2 = do
  lift $ print "start"
  throwError "exception occured"
  lift $ print "finish!"

exceptExample3 :: ExceptT String IO ()
exceptExample3 = do
  lift $ print "start"
  result <- testGetch2 "Aa0"
  lift $ print $ "finish with result: " ++ show result

exceptExample4 :: ExceptT String IO ()
exceptExample4 = do
  lift $ print "start"
  result <- testGetch2 "Aa"
  lift $ print $ "finish with result: " ++ show result

errorHandler :: String -> ExceptT String IO ()
errorHandler e = do
  lift $ print "caught in errorHandler"
  lift $ print e

main05 :: IO ()
main05 = do
  runExceptT $ do
    exceptExample2 `catchError` errorHandler
    exceptExample3 `catchError` errorHandler
    exceptExample4 `catchError` errorHandler
  return ()
