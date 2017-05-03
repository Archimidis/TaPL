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
    ]

testSuccessfulParse :: (String, Term) -> Assertion
testSuccessfulParse (input, expected) =
  parseUntypedLambda input @?= Right expected

validExamplesForParse :: [(String, Term)]
validExamplesForParse =
  [ ("x", TmVar 0 0) -- XXX: should throw error
  , ("y", TmVar 0 0) -- XXX: should throw error
  , ("λy.y", TmAbs "y" (TmVar 0 0))
  , ("λx.x", TmAbs "x" (TmVar 0 0))
  , ("λx.λy.x", TmAbs "x" (TmAbs "y" (TmVar 1 0)))
  , ("λx.λx.x", TmAbs "x" (TmAbs "x" (TmVar 1 0)))
  , ("λy.λx.x", TmAbs "y" (TmAbs "x" (TmVar 0 0)))
  , ("(λx.x) (λx.x)", TmApp (TmAbs "x" (TmVar 0 0)) (TmAbs "x" (TmVar 0 0)))
  , ("λx.λy.(x y)", TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 0) (TmVar 0 0))))
  , ( "λx.λy.x (y x)"
    , TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 0) (TmApp (TmVar 0 0) (TmVar 1 0)))))
  , ( "λs.λz.s (s z)"
    , TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 0) (TmApp (TmVar 1 0) (TmVar 0 0)))))
  , ( "(λx.(λx.x)) (λx.x)"
    , TmApp (TmAbs "x" (TmAbs "x" (TmVar 1 0))) (TmAbs "x" (TmVar 0 0)))
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
                    (TmApp (TmVar 3 0) (TmVar 1 0))
                    (TmApp (TmApp (TmVar 2 0) (TmVar 0 0)) (TmVar 1 0)))))))
  ]

-- Testing same application expression inside abstraction with parenthesis in
-- different locations
sameAppWithDifferentParenthesis :: [(String, Term)]
sameAppWithDifferentParenthesis =
  [ ("λx.(x (λx.x))", TmAbs "x" (TmApp (TmVar 0 0) (TmAbs "x" (TmVar 1 0))))
  , ("λx.(x λx.x)", TmAbs "x" (TmApp (TmVar 0 0) (TmAbs "x" (TmVar 1 0))))
  , ("λx.x (λx.x)", TmAbs "x" (TmApp (TmVar 0 0) (TmAbs "x" (TmVar 1 0))))
  , ("λx.x λx.x", TmAbs "x" (TmApp (TmVar 0 0) (TmAbs "x" (TmVar 1 0))))
  ]

yCombinator :: [(String, Term)]
yCombinator =
  [ ( "λf.(λx.f (x x)) (λx.f (x x))" -- Y combinator (call-by-name)
    , TmAbs
        "f"
        (TmApp
           (TmAbs "x" (TmApp (TmVar 1 0) (TmApp (TmVar 0 0) (TmVar 0 0))))
           (TmAbs "x" (TmApp (TmVar 1 0) (TmApp (TmVar 0 0) (TmVar 0 0))))))
  , ( "λf.(λx.f (λu.x x u)) (λx.f (λu.x x u))" -- Y combinator (call-by-value)
    , TmAbs
        "f"
        (TmApp
           (TmAbs
              "x"
              (TmApp
                 (TmVar 1 0)
                 (TmAbs "u" (TmApp (TmApp (TmVar 1 0) (TmVar 1 0)) (TmVar 0 0)))))
           (TmAbs
              "x"
              (TmApp
                 (TmVar 1 0)
                 (TmAbs "u" (TmApp (TmApp (TmVar 1 0) (TmVar 1 0)) (TmVar 0 0)))))))
  ]
