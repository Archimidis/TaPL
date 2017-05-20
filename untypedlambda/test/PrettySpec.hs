module PrettySpec
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Pretty (renderExpression)
import Syntax (Term(..))

tests :: TestTree
tests =
  testGroup
    "Pretty"
    [ renderExpressionWithoutRenaming
    , renderExpressionWithRenaming
    , renderYCombinator
    , renderInvalidExpressions
    ]

renderExpressionWithoutRenaming :: TestTree
renderExpressionWithoutRenaming =
  testGroup
    "render expressions without renaming need"
    [ testCase "render λy.y" $
      renderExpression (TmAbs "y" (TmVar 0 1)) @?= "(λy.y)"
    , testCase "render λy.y" $
      renderExpression (TmAbs "y" (TmVar 0 1)) @?= "(λy.y)"
    , testCase "render λx.x" $
      renderExpression (TmAbs "x" (TmVar 0 1)) @?= "(λx.x)"
    , testCase "render λx.λy.x" $
      renderExpression (TmAbs "x" (TmAbs "y" (TmVar 1 2))) @?= "(λx.(λy.x))"
    , testCase "render λy.λx.x" $
      renderExpression (TmAbs "y" (TmAbs "x" (TmVar 0 2))) @?= "(λy.(λx.x))"
    , testCase "render (λx.x) (λx.x)" $
      renderExpression (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmVar 0 1))) @?=
      "((λx.x) (λx.x))"
    , testCase "render λx.λy.(x y)" $
      renderExpression (TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 2) (TmVar 0 2)))) @?=
      "(λx.(λy.(x y)))"
    , testCase "render λx.λy.x (y x)" $
      renderExpression
        (TmAbs
           "x"
           (TmAbs "y" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 1 2))))) @?=
      "(λx.(λy.(x (y x))))"
    , testCase "render λs.λz.s (s z)" $
      renderExpression
        (TmAbs
           "s"
           (TmAbs "z" (TmApp (TmVar 1 2) (TmApp (TmVar 1 2) (TmVar 0 2))))) @?=
      "(λs.(λz.(s (s z))))"
    , testCase "render λm.λn.λs.λz.m s (n z s)" $
      renderExpression
        (TmAbs
           "m"
           (TmAbs
              "n"
              (TmAbs
                 "s"
                 (TmAbs
                    "z"
                    (TmApp
                       (TmApp (TmVar 3 4) (TmVar 1 4))
                       (TmApp (TmApp (TmVar 2 4) (TmVar 0 4)) (TmVar 1 4))))))) @?=
      "(λm.(λn.(λs.(λz.((m s) ((n z) s))))))"
    ]

renderExpressionWithRenaming :: TestTree
renderExpressionWithRenaming =
  testGroup
    "render expressions that need variable renaming"
    [ testCase "render λx.λx.x" $
      renderExpression (TmAbs "x" (TmAbs "x" (TmVar 1 2))) @?= "(λx.(λx'.x))"
    , testCase "render (λx.(λx.x)) (λx.x)" $
      renderExpression
        (TmApp (TmAbs "x" (TmAbs "x" (TmVar 1 2))) (TmAbs "x" (TmVar 0 1))) @?=
      "((λx.(λx'.x)) (λx.x))"
    , testCase "render λx.λx.λx.λx.λx.λx.λx.x" $
      renderExpression
        (TmAbs
           "x"
           (TmAbs
              "x"
              (TmAbs
                 "x"
                 (TmAbs "x" (TmAbs "x" (TmAbs "x" (TmAbs "x" (TmVar 6 7)))))))) @?=
      "(λx.(λx'.(λx''.(λx'''.(λx''''.(λx'''''.(λx''''''.x)))))))"
    ]

renderYCombinator :: TestTree
renderYCombinator =
  testGroup
    "render Y combinator"
    [ testCase "render (call-by-name) λf.(λx.f (x x)) (λx.f (x x))" $
      renderExpression
        (TmAbs
           "f"
           (TmApp
              (TmAbs "x" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 0 2))))
              (TmAbs "x" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 0 2)))))) @?=
      "(λf.((λx.(f (x x))) (λx.(f (x x)))))"
    , testCase "render (call-by-value) λf.(λx.f (λu.x x u)) (λx.f (λu.x x u))" $
      renderExpression
        (TmAbs
           "f"
           (TmApp
              (TmAbs
                 "x"
                 (TmApp
                    (TmVar 1 2)
                    (TmAbs
                       "u"
                       (TmApp (TmApp (TmVar 1 3) (TmVar 1 3)) (TmVar 0 3)))))
              (TmAbs
                 "x"
                 (TmApp
                    (TmVar 1 2)
                    (TmAbs
                       "u"
                       (TmApp (TmApp (TmVar 1 3) (TmVar 1 3)) (TmVar 0 3))))))) @?=
      "(λf.((λx.(f (λu.((x x) u)))) (λx.(f (λu.((x x) u))))))"
    ]

renderInvalidExpressions :: TestTree
renderInvalidExpressions =
  testGroup
    "render invalid expressions"
    [ testCase "render invalid de bruijn index" $
      renderExpression (TmAbs "x" (TmVar 100 0)) @?= "(λx.[bad index])"
    , testCase "render invalid context length in TmVar" $
      renderExpression (TmAbs "x" (TmVar 0 100)) @?= "(λx.[bad index])"
    , testCase "render lambda abstraction parsed with wrong term" $
      renderExpression (TmAbs "x" TmWrong) @?= "(λx.[bad term])"
    , testCase "render application parsed with 2 wrong term" $
      renderExpression (TmApp TmWrong TmWrong) @?= "([bad term] [bad term])"
    ]
