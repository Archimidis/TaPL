module Syntax (Term(..)) where

type Name = String
type Index = Int
type ContextTotalLength = Int

data Term = TmVar Index ContextTotalLength
          | TmAbs Name Term
          | TmApp Term Term
  deriving (Show, Eq)
