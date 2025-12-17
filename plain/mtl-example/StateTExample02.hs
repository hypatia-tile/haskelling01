import Control.Monad.State.Strict

-- In this example, we will modify the State

-- | For observation, I define a simple newtype wrapper
newtype MyVal a = MyVal {getMyVal :: a} deriving (Eq)

instance (Show a) => Show (MyVal a) where
  show (MyVal v) = "Value is " ++ show v

{- | Use MyVal to be involved in StateT
  So it is My `Value`
-}
myFunc :: Int -> MyVal Int
myFunc x = MyVal $ x * 2

{- | Either Monad may good for Transformer example
  because it can represent failure case easily
-}
myStateT :: StateT Int (Either String) (MyVal Int)
myStateT = gets myFunc

{- | I merely show mapping function switch the context
   from Either monad to IO monad. No other meaning here.
   Weak semantics for state in failure case, just return -1 as state.
   It may be urgy, but just for demonstration
-}
myMapFunc :: Either String (MyVal Int, Int) -> IO (String, Int)
myMapFunc (Left err) = return ("Error: " ++ err, -1)
myMapFunc (Right (MyVal n, v)) = return ("Success: " ++ show n, v)

{- | I completely forgot StateT monad can be generated
   by directly receive a function, so trivial example here.
-}
evenState :: StateT Int (Either String) (MyVal Int)
evenState = StateT $ \a ->
  if even a
    then
      Left "Odd number encountered"
    else
      Right (MyVal (a + 1), a)

{- | Now I have change the context from Either to IO
    The context of evenstate is certainly inhevited by mapper function
-}
main :: IO ()
main = do
  let initialState = myStateT
  let ioState = mapStateT myMapFunc initialState
  result <- runStateT ioState 10
  putStrLn $ "Main Result: " ++ show result
  let leftState = mapStateT myMapFunc evenState
  result2 <- runStateT leftState 3
  putStrLn $ "Main Result with odd state: " ++ show result2
  result3 <- runStateT leftState 4
  putStrLn $ "Main Result with even state: " ++ show result3
