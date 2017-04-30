module Main where

import Test.Tasty (defaultMain, testGroup)

import ExampleSpec

main :: IO ()
main = defaultMain $ testGroup "Tests" [ ExampleSpec.tests ]
