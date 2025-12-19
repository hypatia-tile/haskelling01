-- I want to certain the behavior of StateT monad in do AnnotationWrapper

import Control.Monad.State.Strict

getch :: StateT [a] Maybe a
getch = StateT getch
 where
  getch (x : xs) = Just (x, xs)
  getch _ = Nothing

{- | Use 'getch' three times in a StateT-context do-block
      In first three line:
        getch returns a value of [a] -> Maybe (a, [a])
        the result of getch is evaluated as 'a' by the '<-' operator

      In the last line:
        the return value is of type 'a'
-}
get3 :: [a] -> Maybe [a]
get3 = evalStateT $ do
  x1 <- getch
  x2 <- getch
  x3 <- getch
  return [x1, x2, x3]
