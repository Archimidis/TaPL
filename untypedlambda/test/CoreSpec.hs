module CoreSpec
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Core
import Syntax

tests :: TestTree
tests = testGroup "Core" [evalUnits]

evalUnits :: TestTree
evalUnits =
  testGroup
    "eval1"
    [ testCase "eval1 (λx.x) (λx.x)" $
      eval1 (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmVar 0 1))) @?=
      TmAbs "x" (TmVar 0 1)
    , testCase "eval1 (λx.(λx.x)) (λx.x)" $
      eval1 (TmApp (TmAbs "x" (TmAbs "x" (TmVar 1 2))) (TmAbs "x" (TmVar 0 1))) @?=
      TmAbs "x" (TmAbs "x" (TmVar 0 2))
    , testCase "eval1 (λx.λy.x) (λx.x)" $
      eval1 (TmApp (TmAbs "x" (TmAbs "y" (TmVar 1 2))) (TmAbs "x" (TmVar 0 1))) @?=
      TmAbs "y" (TmAbs "x" (TmVar 0 2))
    ]
