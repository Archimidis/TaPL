module Pretty
  ( renderExpression
  ) where

import Syntax (Term(..))
import Text.PrettyPrint

type Context = [String]

type VarName = String

pickFreshName :: Context -> VarName -> (Context, VarName)
pickFreshName ctx x =
  if x `elem` ctx
    then pickFreshName ctx (x ++ "'")
    else (x : ctx, x)

printTerm :: Context -> Term -> Doc
printTerm ctx (TmApp t1 t2) =
  lparen <> printTerm ctx t1 <+> printTerm ctx t2 <> rparen
printTerm ctx (TmAbs x t1) =
  lparen <> char 'λ' <> text x' <> char '.' <> printTerm ctx' t1 <> rparen
  where
    (ctx', x') = pickFreshName ctx x
printTerm ctx (TmVar index n) =
  if length ctx == n
    then text (ctx !! index)
    else text "[bad index]"
printTerm _ TmWrong = text "[bad term]"

renderExpression :: Term -> String
renderExpression expr = render $ printTerm [] expr
