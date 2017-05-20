module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified CoreSpec
import qualified ParserSpec
import qualified PrettySpec

main :: IO ()
main =
  defaultMain $
  testGroup "Tests" [CoreSpec.tests, ParserSpec.tests, PrettySpec.tests]
