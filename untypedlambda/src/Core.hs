module Core
  ( evaluateOneStep
  , eval1
  ) where

import Syntax

termShift :: Int -> Term -> Term
termShift d = walk 0
  where
    walk c t =
      case t of
        (TmVar index context) ->
          if index >= c
            then TmVar (index + d) (context + d)
            else TmVar index (context + d)
        (TmAbs name term) -> TmAbs name (walk (c + 1) term)
        (TmApp t1 t2) -> TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where
    walk c t =
      case t of
        (TmVar index context) ->
          if index == j + c
            then termShift c s
            else TmVar index context
        (TmAbs name t1) -> TmAbs name (walk (c + 1) t1)
        (TmApp t1 t2) -> TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isValue :: Term -> Bool
isValue term =
  case term of
    (TmAbs _ _) -> True
    _ -> False

evaluateOneStep :: Term -> Term
evaluateOneStep (TmApp (TmAbs _ t12) v2)
  | isValue v2 = termSubstTop v2 t12
evaluateOneStep (TmApp v1 t2)
  | isValue v1 =
    let t2' = eval1 t2
    in TmApp v1 t2'
evaluateOneStep (TmApp t1 t2) =
  let t1' = eval1 t1
  in TmApp t1' t2
evaluateOneStep _ = TmWrong

eval1 :: Term -> Term
eval1 term
  | t' == TmWrong = term
  | isValue t' = t'
  | otherwise = eval1 t'
  where
    t' = evaluateOneStep term
