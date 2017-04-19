module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer =
  Tok.makeTokenParser
    emptyDef
    { Tok.reservedNames = ["true", "false", "if", "then", "else"]
    , Tok.reservedOpNames = ["0", "succ", "pred", "iszero"]
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
