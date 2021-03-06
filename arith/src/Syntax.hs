module Syntax (Term(..)) where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmWrong
  deriving (Show, Eq)

