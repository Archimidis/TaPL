module Main
  ( main
  ) where

import Control.Applicative ((<$>))
import System.Console.Haskeline

import Text.Parsec (ParseError)
import Text.PrettyPrint (Doc, int, text, render)

import qualified Core as C
import qualified Parser as P
import Syntax (Term(..))

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
readEval input = (render . pretify . C.eval1) <$> P.parseArith input

pretify :: Term -> Doc
pretify term =
  case term of
    TmTrue -> text "true"
    TmFalse -> text "false"
    TmZero -> text "0"
    number@(TmSucc _) -> int $ calculate number
    _ -> text "Invalid input"

calculate :: Term -> Int
calculate term =
  case term of
    TmZero -> 0
    TmSucc t -> 1 + calculate t
    _ -> 0
