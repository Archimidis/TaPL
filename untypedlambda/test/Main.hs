module Main where

import Test.Tasty (defaultMain, testGroup)

import CoreSpec
import ParserSpec

main :: IO ()
main = defaultMain $ testGroup "Tests" [CoreSpec.tests, ParserSpec.tests]
