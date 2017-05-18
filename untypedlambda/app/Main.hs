module Main
  ( main
  ) where

import Control.Applicative ((<$>))
import System.Console.Haskeline

import Text.Parsec (ParseError)

import qualified Core as C
import qualified Parser as P
import Syntax (Term(..))

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  input <- getInputLine "lambda> "
  case input of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "q" -> return ()
    Just "test" -> showResult (cSucc ++ " " ++ c0) >> loop
    Just "c0" -> showResult c0 >> loop
    Just "c1" -> showResult c1 >> loop
    Just s -> do
      case readEval s of
        Left msg -> outputStrLn $ show msg
        Right expr -> outputStrLn $ show expr
      loop

showResult s =
  case readEval s of
    Left msg -> outputStrLn $ show msg
    Right expr -> outputStrLn $ show expr

readEval :: String -> Either ParseError Term
readEval input = C.eval1 <$> P.parseUntypedLambda input

{- Church Encoding for numbers 0 and 1 and operations plus and succ -}
c0 :: String
c0 = "(λf.λx.x)"

c1 :: String
c1 = "(λf.λx.f x)"

cPlus :: String
cPlus = "(λm.λn.λf.λx.m f (n f x))"

cSucc :: String
cSucc = "(λn.λf.λx.f (n f x))"
