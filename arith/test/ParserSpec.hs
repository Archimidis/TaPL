module ParserSpec
  ( tests
  ) where

import Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Gens (genValidExpr)
import TestUtils (testWithProvider)

import Parser (parseArith)
import Pretty (renderExpression)
import Syntax

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ testProperty "expression round trip" prop_expr_round_trip
    , testWithProvider "parseArith" testParse validExamplesForParse
    ]

prop_expr_round_trip :: Property
prop_expr_round_trip =
  property $ do
    expr <- forAll genValidExpr
    tripping expr renderExpression parseArith

testParse :: (String, Term) -> Assertion
testParse (input, expected) = parseArith input @?= Right expected

validExamplesForParse :: [(String, Term)]
validExamplesForParse =
  [ ("true", TmTrue)
  , ("false", TmFalse)
  , ("0", TmZero)

  , ("succ 0", TmSucc TmZero)
  , ("succ (succ 0)", TmSucc (TmSucc TmZero))
  , ("succ (succ (succ 0))", TmSucc (TmSucc (TmSucc TmZero)))
  , ("succ (succ (succ (succ 0)))", TmSucc (TmSucc (TmSucc (TmSucc TmZero))))

  , ("pred 0", TmPred TmZero)
  , ("pred (pred 0)", TmPred (TmPred TmZero))
  , ("pred (pred (pred 0))", TmPred (TmPred (TmPred TmZero)))
  , ("pred (pred (pred (pred 0)))", TmPred (TmPred (TmPred (TmPred TmZero))))
  , ("pred (succ 0)", TmPred (TmSucc TmZero))

  , ("succ (pred 0)", TmSucc (TmPred TmZero))
  , ("succ (pred (succ 0))", TmSucc (TmPred (TmSucc TmZero)))
  , ("pred (succ (succ (succ 0)))", TmPred (TmSucc (TmSucc (TmSucc TmZero))))
  , ("succ (succ (pred (succ 0)))", TmSucc (TmSucc (TmPred (TmSucc TmZero))))
  , ("pred (succ (pred (succ 0)))", TmPred (TmSucc (TmPred (TmSucc TmZero))))

  , ("iszero 0", TmIsZero TmZero)
  , ("iszero (succ 0)", TmIsZero (TmSucc TmZero))
  , ("iszero (pred (succ 0))", TmIsZero (TmPred (TmSucc TmZero)))

  , ("if true then 0 else 0", TmIf TmTrue TmZero TmZero)
  , ("if false then 0 else succ 0", TmIf TmFalse TmZero (TmSucc TmZero))
  , ("if true then true else false", TmIf TmTrue TmTrue TmFalse)
  , ( "if iszero 0 then pred (succ 0) else succ (succ 0)"
    , TmIf (TmIsZero TmZero) (TmPred (TmSucc TmZero)) (TmSucc (TmSucc TmZero)))
  , ( "if iszero 0 then (pred (succ 0)) else (succ (succ 0))"
    , TmIf (TmIsZero TmZero) (TmPred (TmSucc TmZero)) (TmSucc (TmSucc TmZero)))
  , ( "if (iszero 0) then (pred (succ 0)) else (succ (succ 0))"
    , TmIf (TmIsZero TmZero) (TmPred (TmSucc TmZero)) (TmSucc (TmSucc TmZero)))
  , ( "if (iszero 0) then (if true then 0 else succ 0) else (iszero (succ (succ 0)))"
    , TmIf
        (TmIsZero TmZero)
        (TmIf TmTrue TmZero (TmSucc TmZero))
        (TmIsZero (TmSucc (TmSucc TmZero))))
  , ( "iszero (if (iszero 0) then (if true then 0 else succ 0) else (iszero (succ (succ 0))))"
    , TmIsZero
        (TmIf
           (TmIsZero TmZero)
           (TmIf TmTrue TmZero (TmSucc TmZero))
           (TmIsZero (TmSucc (TmSucc TmZero)))))

  , ("                      false           ", TmFalse)
  , ("if true\n\tthen 0\n\telse 0\n", TmIf TmTrue TmZero TmZero)
  ]
