module Gens
  ( genValidExpr
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Syntax

genNumber :: Gen Term
genNumber =
  Gen.recursive
    Gen.choice
      -- Non-recursive generators
    [pure TmZero]
      -- Recursive generators
    [TmSucc <$> genNumber, TmPred <$> genNumber]

genBool :: Gen Term
genBool = Gen.choice [pure TmTrue, pure TmFalse, pure TmZero]

genValidExpr :: Gen Term
genValidExpr =
  Gen.recursive
    Gen.choice
      -- Non-recursive generators
    [genBool, genNumber]
      -- Recursive generators
    [TmIf <$> genBool <*> genValidExpr <*> genValidExpr, TmIsZero <$> genNumber]
