module Main
  ( main
  ) where

import Control.Applicative ((<$>))
import System.Console.Haskeline

import Text.Parsec (ParseError)

import qualified Core as C
import qualified Parser as P
import qualified Pretty (renderExpression)

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  input <- getInputLine "arith> "
  case input of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "q" -> return ()
    Just s -> do
      case readEval s of
        Left msg -> outputStrLn $ show msg
        Right expr -> outputStrLn $ show expr
      loop

readEval :: String -> Either ParseError String
readEval input = (Pretty.renderExpression . C.eval1) <$> P.parseArith input
