module Main where

import Test.Tasty (defaultMain, testGroup)

import CoreSpec

main :: IO ()
main = defaultMain $ testGroup "Tests" [CoreSpec.tests]
