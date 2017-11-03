module CoreSpec
  ( tests
  ) where

import Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Gens (genValidExpr)

import Core
import Syntax

tests :: TestTree
tests =
  testGroup
    "Core"
    [ testProperty "eval1 idempotent" prop_eval1_idempotent
    , testProperty "evalb idempotent" prop_evalb_idempotent
    , evaluateOneStepUnits
    , evalUnits
    , evaluateBigStepUnits
    , evalbUnits
    ]

prop_eval1_idempotent :: Property
prop_eval1_idempotent =
  property $ do
    expr <- forAll genValidExpr
    eval1 (eval1 expr) === eval1 expr

prop_evalb_idempotent :: Property
prop_evalb_idempotent =
  property $ do
    expr <- forAll genValidExpr
    evalb (evalb expr) === evalb expr

evaluateOneStepUnits :: TestTree
evaluateOneStepUnits = testGroup "evaluateOneStep"
  [ testCase "will evaluate to the else expression if predicate is false" $
      evaluateOneStep (TmIf TmFalse TmZero (TmSucc TmZero)) @?= TmSucc TmZero
  , testCase "will evaluate to the then expression if predicate is true" $
      evaluateOneStep (TmIf TmTrue TmZero (TmSucc TmZero)) @?= TmZero
  , testCase "will evaluate first the if expression if it is not a value" $
      evaluateOneStep (TmIf (TmIsZero TmZero) TmZero (TmSucc TmZero)) @?=
          TmIf TmTrue TmZero (TmSucc TmZero)
  , testCase "will evaluate first the if expression if it contains another if expression" $
      evaluateOneStep (TmIf (TmIf TmTrue TmTrue TmFalse)
                            TmZero
                            (TmSucc TmZero))
                      @?=
                      TmIf TmTrue
                           TmZero
                           (TmSucc TmZero)

  , testCase "will evaluate a complex expression" $
      evaluateOneStep (TmIf (TmIf (TmIsZero (TmSucc TmZero)) TmTrue TmFalse)
                            TmZero
                            (TmSucc TmZero))
                      @?=
                      TmIf (TmIf TmFalse TmTrue TmFalse)
                           TmZero
                           (TmSucc TmZero)


  , testCase "will evaluate TmSucc if it contains an expression" $
      evaluateOneStep (TmSucc (TmIf TmTrue TmZero (TmSucc TmZero))) @?= TmSucc TmZero
  , testCase "will evaluate TmPred of zero to zero" $
      evaluateOneStep (TmPred TmZero) @?= TmZero
  , testCase "will evaluate TmPred of TmSucc of zero" $
      evaluateOneStep (TmPred (TmSucc TmZero)) @?= TmZero
  , testCase "will evaluate TmPred of TmSucc of another number" $
      evaluateOneStep (TmPred (TmSucc (TmSucc TmZero))) @?= TmSucc TmZero
  , testCase "will evaluate TmPred if it contains an expression" $
      evaluateOneStep (TmPred (TmIf TmTrue TmZero (TmSucc TmZero))) @?= TmPred TmZero

  , testCase "will evaluate to TmTrue when TmIsZero holds TmZero" $
      evaluateOneStep (TmIsZero TmZero) @?= TmTrue
  , testCase "will evaluate TmIsZero to TmFalse if it holds something grater than zero" $
      evaluateOneStep (TmIsZero (TmSucc (TmSucc TmZero))) @?= TmFalse
  , testCase "will evaluate TmIsZero if it contains an expression" $
      evaluateOneStep (TmIsZero (TmIf TmTrue TmZero TmZero)) @?= TmIsZero TmZero

  , testCase "will evaluate if with TmWrong in predicate expression to TmWrong" $
      evaluateOneStep (TmIf TmWrong TmZero TmZero) @?= TmWrong
  , testCase "will evaluate if with TmZero in predicate expression to TmWrong" $
      evaluateOneStep (TmIf TmZero TmZero TmZero) @?= TmWrong
  , testCase "will evaluate if with a successor in predicate expression to TmWrong" $
      evaluateOneStep (TmIf (TmSucc (TmSucc TmZero)) TmZero TmZero) @?= TmWrong

  , testCase "will evaluate TmSucc with TmWrong to TmWrong" $
      evaluateOneStep (TmSucc TmWrong) @?= TmWrong
  , testCase "will evaluate TmSucc with TmTrue to TmWrong" $
      evaluateOneStep (TmSucc TmTrue) @?= TmWrong
  , testCase "will evaluate TmSucc with TmFalse to TmWrong" $
      evaluateOneStep (TmSucc TmFalse) @?= TmWrong

  , testCase "will evaluate TmPred with TmWrong to TmWrong" $
      evaluateOneStep (TmPred TmWrong) @?= TmWrong
  , testCase "will evaluate TmPred with TmTrue to TmWrong" $
      evaluateOneStep (TmPred TmTrue) @?= TmWrong
  , testCase "will evaluate TmPred with TmFalse to TmWrong" $
      evaluateOneStep (TmPred TmFalse) @?= TmWrong

  , testCase "will evaluate TmIsZero with TmWrong to TmWrong" $
      evaluateOneStep (TmIsZero TmWrong) @?= TmWrong
  , testCase "will evaluate TmIsZero with TmTrue to TmWrong" $
      evaluateOneStep (TmIsZero TmTrue) @?= TmWrong
  , testCase "will evaluate TmIsZero with TmFalse to TmWrong" $
      evaluateOneStep (TmIsZero TmFalse) @?= TmWrong

  , testCase "will evaluate value TmTrue to itself" $
      evaluateOneStep TmTrue @?= TmTrue
  , testCase "will evaluate value TmFalse to itself" $
      evaluateOneStep TmFalse @?= TmFalse
  , testCase "will evaluate value TmZero to itself" $
      evaluateOneStep TmZero @?= TmZero
  , testCase "will evaluate value TmSucc TmZero to itself" $
      evaluateOneStep (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc TmZero))))) @?=
          TmSucc (TmSucc (TmSucc (TmSucc (TmSucc TmZero))))
  , testCase "will evaluate value TmWrong to itself" $
      evaluateOneStep TmWrong @?= TmWrong
  ]

evalUnits :: TestTree
evalUnits = testGroup "eval1"
  [ testCase "eval1 expression 1" $
      eval1 (TmPred (TmIf (TmIsZero (TmSucc (TmSucc TmZero)))
                          TmZero
                          (TmSucc TmZero)))
      @?= TmZero
  , testCase "eval1 expression 2" $
      eval1 (TmPred (TmIf (TmIsZero TmZero)
                          TmZero
                          (TmSucc TmZero)))

      @?= TmZero
  , testCase "eval1 invalid expression 1" $
      eval1 (TmPred (TmIf TmZero TmZero TmZero)) @?= TmPred TmWrong
  , testCase "eval1 invalid expression 2" $
      eval1 (TmPred (TmIf (TmIsZero TmTrue)
                          (TmSucc TmZero)
                          (TmSucc TmZero)))
      @?= TmPred TmWrong
  ]

evaluateBigStepUnits :: TestTree
evaluateBigStepUnits = testGroup "evaluateBigStep"
  [ testCase "will evaluate value TmTrue to itself" $
      evaluateBigStep TmTrue @?= TmTrue
  , testCase "will evaluate value TmFalse to itself" $
      evaluateBigStep TmFalse @?= TmFalse
  , testCase "will evaluate value TmZero to itself" $
      evaluateBigStep TmZero @?= TmZero
  , testCase "will evaluate value TmSucc TmZero to itself" $
      evaluateBigStep (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc TmZero))))) @?=
          TmSucc (TmSucc (TmSucc (TmSucc (TmSucc TmZero))))
  , testCase "will evaluate value TmWrong to itself" $
      evaluateBigStep TmWrong @?= TmWrong

  , testCase "will evaluate if's then expression if predicate evaluates to TmTrue" $
      evaluateBigStep (TmIf (TmIsZero TmZero)
                            TmZero
                            (TmSucc TmZero))
      @?= TmZero
  , testCase "will evaluate if's else expression if predicate evaluates to TmFalse" $
      evaluateBigStep (TmIf (TmIsZero (TmSucc (TmSucc TmZero)))
                            TmZero
                            (TmSucc TmZero))
      @?= TmSucc TmZero
  , testCase "will evaluate to TmWrong if predicate does not evaluate to a bool" $
      evaluateBigStep (TmIf (TmSucc (TmSucc TmZero))
                            TmZero
                            (TmSucc TmZero))
      @?= TmWrong

  , testCase "will evaluate TmSucc's expression if it's numeric value" $
      evaluateBigStep (TmSucc (TmIf TmTrue
                                    (TmSucc TmZero)
                                    (TmSucc (TmSucc TmZero))))
      @?= TmSucc (TmSucc TmZero)
  , testCase "will evaluate to TmWrong if TmSucc's expression is not a number" $
      evaluateBigStep (TmSucc (TmIf TmTrue TmTrue TmFalse)) @?= TmWrong

  , testCase "will evaluate TmPred to TmZero if it contains TmZero" $
      evaluateBigStep (TmPred TmZero) @?= TmZero
  , testCase "will evaluate TmPred to the previous number" $
      evaluateBigStep (TmPred (TmSucc (TmSucc TmZero))) @?= TmSucc TmZero
  , testCase "will evaluate TmPred to TmWrong if subexpression is not a number" $
      evaluateBigStep (TmPred TmWrong) @?= TmWrong
  , testCase "will evaluate TmPred to TmWrong if subexpression does not evaluate to a number" $
      evaluateBigStep (TmPred (TmIf (TmIsZero TmZero) TmFalse TmTrue)) @?= TmWrong

  , testCase "will evaluate to TmTrue if TmIsZero contains a TmZero" $
      evaluateBigStep (TmIsZero TmZero) @?= TmTrue
  , testCase "will evaluate to TmTrue if TmIsZero contains an expression evaluating to TmZero" $
      evaluateBigStep (TmIsZero (TmPred (TmSucc TmZero))) @?= TmTrue
  , testCase "will evaluate to TmWrong if TmIsZero contains a TmWrong" $
      evaluateBigStep (TmIsZero TmWrong) @?= TmWrong
  , testCase "will evaluate to TmWrong if TmIsZero subexpression does not evaluate to a number" $
      evaluateBigStep (TmIsZero (TmIf (TmIsZero TmZero) TmTrue TmTrue)) @?= TmWrong
  ]


evalbUnits :: TestTree
evalbUnits = testGroup "evalb"
  [ testCase "evalb expression 1" $
      evalb (TmPred (TmIf (TmIsZero (TmSucc (TmSucc TmZero)))
                          TmZero
                          (TmSucc TmZero)))
      @?= TmZero
  , testCase "evalb expression 2" $
      evalb (TmPred (TmIf (TmIsZero TmZero)
                          TmZero
                          (TmSucc TmZero)))

      @?= TmZero
  , testCase "evalb invalid expression 1" $
      evalb (TmPred (TmIf TmZero TmZero TmZero)) @?= TmPred (TmIf TmZero TmZero TmZero)
  , testCase "evalb invalid expression 2" $
      evalb (TmPred (TmIf (TmIsZero TmTrue)
                          (TmSucc TmZero)
                          (TmSucc TmZero)))
      @?= TmPred (TmIf (TmIsZero TmTrue)
                       (TmSucc TmZero)
                       (TmSucc TmZero))
  ]
