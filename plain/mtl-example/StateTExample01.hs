import Control.Monad.State.Strict

-- StateT Int IO String represents:
--   Int -> IO (String, Int)
--
-- Here, the state is of type Int:
--   Intuitively the object has an internal state of type Int
-- The innner monad is IO:
--   We are about to handle this object in the IO context
--  The final result type is String:
--    We will finally get a String out of this object
--
-- More generally (and informal):
--   StateT :: StateType -> InnerMonad (ResultType, StateType)
--   F state -> M (result, newState)
-- In above case, the represented function of StateT receives a state of type Int,
-- and return the value, result and new state, which are all determined by the old state.

{- | Pure function that format an Int.
    This is independent of State or IO.
-}
myFunc :: Int -> String
myFunc x = "The value is: " ++ show x

{- | A stateful computation that:
  - reads the current Int state
  - applies 'myFunc' to it
  - does NOT modify the state

  Operationally:
    StateT Int IO String
    ≈ Int -> IO (String, Int)
  TODO: Explain what gets does
-}
myState :: StateT Int IO String
myState = gets myFunc

{- | Run 'myState' with an initial Int
  The final state is discarded using 'evalStateT'
-}
evalMyState :: Int -> IO String
evalMyState = evalStateT myState

main :: IO ()
main = do
  -- Run the stateful computation with initial state 42
  result <- evalMyState 42
  putStrLn result
