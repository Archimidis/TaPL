module Core
  ( evaluateOneStep
  , eval1
  , evaluateBigStep
  , evalb
  ) where

import Syntax

isNumericValue :: Term -> Bool
isNumericValue TmZero = True
isNumericValue (TmSucc t1) = isNumericValue t1
isNumericValue _ = False

isValue :: Term -> Bool
isValue term
  | term == TmTrue = True
  | term == TmFalse = True
  | isNumericValue term = True
  | otherwise = False

{- Implementation of one-step evaluation -}

evaluateOneStep :: Term -> Term
-- TmIf evaluation
evaluateOneStep (TmIf TmWrong _ _) = TmWrong
evaluateOneStep (TmIf t1 _ _)
  | isNumericValue t1 = TmWrong
evaluateOneStep (TmIf TmTrue t2 _) = t2
evaluateOneStep (TmIf TmFalse _ t3) = t3
evaluateOneStep (TmIf t1 t2 t3) = TmIf (evaluateOneStep t1) t2 t3
-- TmSucc evaluation
evaluateOneStep (TmSucc TmWrong) = TmWrong
evaluateOneStep (TmSucc TmTrue) = TmWrong
evaluateOneStep (TmSucc TmFalse) = TmWrong
evaluateOneStep (TmSucc t1) = TmSucc (evaluateOneStep t1)
-- TmPred evaluation
evaluateOneStep (TmPred TmWrong) = TmWrong
evaluateOneStep (TmPred TmTrue) = TmWrong
evaluateOneStep (TmPred TmFalse) = TmWrong
evaluateOneStep (TmPred TmZero) = TmZero
evaluateOneStep (TmPred (TmSucc nv1))
  | isNumericValue nv1 = nv1
evaluateOneStep (TmPred t1) = TmPred (evaluateOneStep t1)
-- TmIsZero evaluation
evaluateOneStep (TmIsZero TmWrong) = TmWrong
evaluateOneStep (TmIsZero TmTrue) = TmWrong
evaluateOneStep (TmIsZero TmFalse) = TmWrong
evaluateOneStep (TmIsZero TmZero) = TmTrue
evaluateOneStep (TmIsZero (TmSucc nv1))
  | isNumericValue nv1 = TmFalse
evaluateOneStep (TmIsZero t1) = TmIsZero (evaluateOneStep t1)
evaluateOneStep term
  | isValue term = term
evaluateOneStep TmWrong = TmWrong

eval1 :: Term -> Term
eval1 term
  | t' == TmWrong = term
  | isValue t' = t'
  | otherwise = eval1 t'
  where
    t' = evaluateOneStep term

{- Implementation of big-step evaluation -}

evaluateBigStep :: Term -> Term
-- B-Value evaluation
evaluateBigStep term
  | isValue term = term
-- B-IfTrue and B-IfFalse evaluation
evaluateBigStep (TmIf t1 t2 t3) =
  case evaluateBigStep t1 of
    TmTrue -> evaluateBigStep t2
    TmFalse -> evaluateBigStep t3
    _ -> TmWrong
-- B-Succ evaluation
evaluateBigStep (TmSucc t1) =
  let nv1 = evaluateBigStep t1
  in if isNumericValue nv1
       then TmSucc nv1
       else TmWrong
-- B-PredZero and B-PredSucc evaluation
evaluateBigStep (TmPred t1) =
  case evaluateBigStep t1 of
    TmZero -> TmZero
    TmSucc nv1 -> nv1
    _ -> TmWrong
-- B-IsZeroZero and B-IsZeroSucc evaluation
evaluateBigStep (TmIsZero t1) =
  case evaluateBigStep t1 of
    TmZero -> TmTrue
    TmSucc _ -> TmFalse
    _ -> TmWrong
evaluateBigStep _ = TmWrong

evalb :: Term -> Term
evalb term
  | t' == TmWrong = term
  | isValue t' = t'
  | otherwise = evalb t'
  where
    t' = evaluateBigStep term
