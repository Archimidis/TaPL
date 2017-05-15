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
    [ testWithProvider
        "parse various lambda expressions"
        testSuccessfulParse
        validExamplesForParse
    , testWithProvider
        "parse same lambda application with various parenthesis combinations"
        testSuccessfulParse
        sameAppWithDifferentParenthesis
    , testWithProvider "parse Y combinator" testSuccessfulParse yCombinator
    , testWithProvider
        "parse expressions used in evaluation tests"
        testSuccessfulParse
        expressionsUsedInEvaluation
    ]

testSuccessfulParse :: (String, Term) -> Assertion
testSuccessfulParse (input, expected) =
  parseUntypedLambda input @?= Right expected

validExamplesForParse :: [(String, Term)]
validExamplesForParse =
  [ ("x", TmVar 0 0) -- XXX: should throw error
  , ("y", TmVar 0 0) -- XXX: should throw error
  , ("λy.y", TmAbs "y" (TmVar 0 1))
  , ("λx.x", TmAbs "x" (TmVar 0 1))
  , ("λx.λy.x", TmAbs "x" (TmAbs "y" (TmVar 1 2)))
  , ("λx.λx.x", TmAbs "x" (TmAbs "x" (TmVar 1 2)))
  , ("λy.λx.x", TmAbs "y" (TmAbs "x" (TmVar 0 2)))
  , ("(λx.x) (λx.x)", TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmVar 0 1)))
  , ("λx.λy.(x y)", TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 2) (TmVar 0 2))))
  , ( "λx.λy.x (y x)"
    , TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 1 2)))))
  , ( "λs.λz.s (s z)"
    , TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 2) (TmApp (TmVar 1 2) (TmVar 0 2)))))
  , ( "(λx.(λx.x)) (λx.x)"
    , TmApp (TmAbs "x" (TmAbs "x" (TmVar 1 2))) (TmAbs "x" (TmVar 0 1)))
  , ( "λm.λn.λs.λz.m s (n z s)"
    , TmAbs
        "m"
        (TmAbs
           "n"
           (TmAbs
              "s"
              (TmAbs
                 "z"
                 (TmApp
                    (TmApp (TmVar 3 4) (TmVar 1 4))
                    (TmApp (TmApp (TmVar 2 4) (TmVar 0 4)) (TmVar 1 4)))))))
  ]

expressionsUsedInEvaluation :: [(String, Term)]
expressionsUsedInEvaluation =
  [ ( "(λx.λy.x) (λx.x)"
    , TmApp (TmAbs "x" (TmAbs "y" (TmVar 1 2))) (TmAbs "x" (TmVar 0 1)))
  ]

-- Testing same application expression inside abstraction with parenthesis in
-- different locations
sameAppWithDifferentParenthesis :: [(String, Term)]
sameAppWithDifferentParenthesis =
  [ ("λx.(x (λx.x))", TmAbs "x" (TmApp (TmVar 0 1) (TmAbs "x" (TmVar 1 2))))
  , ("λx.(x λx.x)", TmAbs "x" (TmApp (TmVar 0 1) (TmAbs "x" (TmVar 1 2))))
  , ("λx.x (λx.x)", TmAbs "x" (TmApp (TmVar 0 1) (TmAbs "x" (TmVar 1 2))))
  , ("λx.x λx.x", TmAbs "x" (TmApp (TmVar 0 1) (TmAbs "x" (TmVar 1 2))))
  ]

yCombinator :: [(String, Term)]
yCombinator =
  [ ( "λf.(λx.f (x x)) (λx.f (x x))" -- Y combinator (call-by-name)
    , TmAbs
        "f"
        (TmApp
           (TmAbs "x" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 0 2))))
           (TmAbs "x" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 0 2))))))
  , ( "λf.(λx.f (λu.x x u)) (λx.f (λu.x x u))" -- Y combinator (call-by-value)
    , TmAbs
        "f"
        (TmApp
           (TmAbs
              "x"
              (TmApp
                 (TmVar 1 2)
                 (TmAbs "u" (TmApp (TmApp (TmVar 1 3) (TmVar 1 3)) (TmVar 0 3)))))
           (TmAbs
              "x"
              (TmApp
                 (TmVar 1 2)
                 (TmAbs "u" (TmApp (TmApp (TmVar 1 3) (TmVar 1 3)) (TmVar 0 3)))))))
  ]
