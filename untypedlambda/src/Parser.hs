module Parser where

import Text.Parsec

import Data.List (elemIndices)

import Syntax

type Context = String

type LCParser = Parsec String Context Term

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

nameToIndex :: Char -> Context -> Int
nameToIndex param context =
  let indices = elemIndices param context
  in if null indices
       then 0
       else last indices

lambdaVar :: LCParser
lambdaVar =
  letter >>= \param ->
    getState >>= \context -> return $ TmVar (nameToIndex param context) 0

lambdaAbs :: LCParser
lambdaAbs =
  string "λ" >> letter >>= \param ->
    char '.' >> modifyState (param :) >> lambdaExpr >>= \t1 ->
      modifyState tail >> return (TmAbs [param] t1)

lambdaApp :: LCParser
lambdaApp =
  noAppExpr >>= \t1 -> space >> noAppExpr >>= \t2 -> return $ TmApp t1 t2

noAppExpr :: LCParser
noAppExpr = parens lambdaExpr <|> lambdaAbs <|> lambdaVar

lambdaExpr :: LCParser
lambdaExpr = chainl1 noAppExpr (space >> return TmApp)

contents :: Parsec String Context a -> Parsec String Context a
contents p = do
  r <- p
  eof
  return r

parseUntypedLambda :: String -> Either ParseError Term
parseUntypedLambda = runParser (contents lambdaExpr) [] "<stdin>"
