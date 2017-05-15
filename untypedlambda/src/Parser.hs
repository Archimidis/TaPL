module Parser where

import Text.Parsec

import Data.List (elemIndices)

import Syntax

{--
 - A context is a list of variables bound in a lambda abstraction and it works
 - just like a stack. For each nested lambda abstraction, its variable is pushed
 - in the front of the context.
 - For example, given the function "λx.λy.x", when both "λx" and "λy" are
 - already parsed and the variable "x" is left, then the current context will be
 - ['y', 'x'].
 - When getting out of the nested abstractions, then the terms are popped from
 - the context.
 -}
type Context = String

type LCParser = Parsec String Context Term

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

{--
 - This function computes the de Bruijn index of a bound variable based on its
 - current context.
 -}
nameToIndex :: Char -> Context -> Int
nameToIndex param context =
  let indices = elemIndices param context
  in if null indices
       then 0
       else last indices

lambdaVar :: LCParser
lambdaVar =
  letter >>= \param ->
    getState >>= \context -> return $ TmVar (nameToIndex param context) (length context)

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
