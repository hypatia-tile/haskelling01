-- In this example, I will demonstrate the use of the ExceptT monad transformer from the mtl library.
import Control.Monad.Except

{- | ExceptT example that always results in an error.
      ExceptT is essentially a wrapper around Either that allows for computations that can fail with an error message.
-}
exceptExample :: ExceptT String IO ()
exceptExample = ExceptT $ return (Left "An Error Occurred")

main :: IO ()
main = do
  result <- runExceptT exceptExample
  case result of
    Left errMsg -> putStrLn $ "Computation failed with error: " ++ errMsg
    Right _ -> putStrLn "Computation succeeded"
