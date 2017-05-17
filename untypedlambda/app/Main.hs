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
    Just s -> do
      case readEval s of
        Left msg -> outputStrLn $ show msg
        Right expr -> outputStrLn $ show expr
      loop

readEval :: String -> Either ParseError Term
readEval input = C.eval1 <$> P.parseUntypedLambda input
