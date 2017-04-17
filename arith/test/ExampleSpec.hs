module ExampleSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Example

tests :: TestTree
tests =
  testGroup "Example" [ exampleTests  ]

exampleTests :: TestTree
exampleTests = testGroup "test"
  [ testCase "will pass" $ example @?= True ]