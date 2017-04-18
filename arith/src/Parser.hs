module Parser where

{-import Control.Applicative ((<$>))-}
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

expr :: Parser Term
expr = Ex.buildExpressionParser [] factor

true :: Parser Term
true = reserved "true" >> return TmTrue

false :: Parser Term
false = reserved "false" >> return TmFalse

zero :: Parser Term
zero = reserved "0" >> return TmZero

successor :: Parser Term
successor = do
  reserved "succ"
  t <- expr
  return $ TmSucc t

predecessor :: Parser Term
predecessor = do
  reserved "pred"
  t <- expr
  return $ TmPred t

isZero :: Parser Term
isZero = do
  reserved "iszero"
  t <- expr
  return $ TmIsZero t

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
      <|> successor
      <|> predecessor
      <|> try isZero
      <|> try ifThenElse
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseArith :: String -> Either ParseError Term
parseArith = parse (contents expr) "<stdin>"
