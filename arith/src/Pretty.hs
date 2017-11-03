module Pretty
  ( renderExpression
  ) where

import Syntax (Term(..))
import Text.PrettyPrint

printTerm :: Term -> Doc
printTerm expr =
  case expr of
    TmTrue -> text "true"
    TmFalse -> text "false"
    (TmIf e1 e2 e3) ->
      text "if" <+> lparen <> printTerm e1 <> rparen <+>
      text "then" <+> lparen <> printTerm e2 <> rparen <+>
      text "else" <+> lparen <> printTerm e3 <> rparen
    TmZero -> text "0"
    TmSucc TmZero -> text "succ 0"
    TmSucc e -> text "succ" <+> lparen <> printTerm e <> rparen
    TmPred e -> text "pred" <+> lparen <> printTerm e <> rparen
    TmIsZero e -> text "iszero" <+> lparen <> printTerm e <> rparen
    TmWrong -> text "[bad term]"

renderExpression :: Term -> String
renderExpression expr = render (printTerm expr)
