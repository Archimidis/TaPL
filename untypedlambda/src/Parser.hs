module Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

type Context = [String]

type LCParser = Parsec String Context Term

var :: LCParser
var = letter >> return (TmVar 0 0)

lambdaAbs :: LCParser
lambdaAbs =
  string "λ" >> letter >>= \param ->
    char '.' >> var >>= \expr -> return (TmAbs [param] expr)

expr :: LCParser
expr = lambdaAbs <|> var

contents :: Parsec String Context a -> Parsec String Context a
contents p = do
  r <- p
  eof
  return r

parseUntypedLambda :: String -> Either ParseError Term
parseUntypedLambda = runParser (contents expr) [] "<stdin>"
