module Main (main) where

import Repl.Arithmetic (ArithExpr (..), evalArith)
import Repl.Command (parseAppCommand)
import Test.HUnit

testEvalArithSimple :: Test
testEvalArithSimple = TestCase $ do
  let result = evalArith [ANum 5, ANum 3, Add]
  assertEqual "5 + 3 should equal 8" (Right 8) result

testParseValidCommand :: Test
testParseValidCommand = TestCase $ do
  let result = parseAppCommand "exit"
  case result of
    Right _ -> return () -- Assuming cmd is of the correct type
    Left _ -> assertFailure "Should parse 'exit' command"

main :: IO ()
main = do
  count <-
    runTestTT $
      TestList
        [ TestLabel "eval simple addition" testEvalArithSimple
        , TestLabel "parse valid command" testParseValidCommand
        ]
  putStrLn $ "Ran " ++ show (cases count) ++ " tests."
  return ()
