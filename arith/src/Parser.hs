module Parser
  ( parseArith
  ) where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

expr :: Parser Term
expr = Ex.buildExpressionParser opTable factor

prefix :: String -> (a -> a) -> Ex.Operator String () Identity a
prefix name fun = Ex.Prefix ( reservedOp name >> return fun )

opTable :: Ex.OperatorTable String () Identity Term
opTable = [
    [ prefix "succ" TmSucc
    , prefix "pred" TmPred
    , prefix "iszero" TmIsZero
    ]
  ]

true :: Parser Term
true = reserved "true" >> return TmTrue

false :: Parser Term
false = reserved "false" >> return TmFalse

zero :: Parser Term
zero = reserved "0" >> return TmZero

ifThenElse :: Parser Term
ifThenElse = do
  reserved "if"
  t1 <- expr
  reserved "then"
  t2 <- expr
  reserved "else"
  t3 <- expr
  return $ TmIf t1 t2 t3

factor :: Parser Term
factor =  true
      <|> false
      <|> zero
      <|> ifThenElse
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseArith :: String -> Either ParseError Term
parseArith = parse (contents expr) "<stdin>"
