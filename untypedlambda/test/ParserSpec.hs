module ParserSpec
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=))

import TestUtils (testWithProvider)

import Parser
import Syntax

tests :: TestTree
tests =
  testGroup
    "Parser"
    [testWithProvider "parseUntypedLambda" testParse validExamplesForParse]

testParse :: (String, Term) -> Assertion
testParse (input, expected) = parseUntypedLambda input @?= Right expected

validExamplesForParse :: [(String, Term)]
validExamplesForParse = []
