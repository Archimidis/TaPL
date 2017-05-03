module TestUtils
  ( testWithProvider
  ) where

import Test.HUnit (Assertion)
import Test.Tasty (TestTree, testGroup, TestName)
import Test.Tasty.HUnit (testCase)
import Text.Printf (PrintfArg, printf)

testWithProvider
  :: PrintfArg t1
  => TestName -> ((t1, t) -> Assertion) -> [(t1, t)] -> TestTree
testWithProvider testGroupName testFunction =
  testGroup testGroupName . map createTest
  where
    createTest dataSet@(input, _) =
      testCase (printf "%s" input) (testFunction dataSet)
