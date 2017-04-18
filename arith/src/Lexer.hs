module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer =
  Tok.makeTokenParser
    emptyDef
    { Tok.reservedNames =
        ["true", "false", "0", "succ", "pred", "iszero", "if", "then", "else"]
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer
