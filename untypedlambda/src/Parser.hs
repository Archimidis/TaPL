module Parser (parseUntypedLambda) where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

parseUntypedLambda :: String -> Either ParseError Term
parseUntypedLambda _ = undefined
