module Main
  ( main
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import System.Console.Haskeline

import qualified Core as C
import qualified Parser as P
import Pretty (renderExpression)

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  input <- getInputLine "lambda> "
  case input of
    Nothing -> return ()
    Just "" -> loop
    Just "quit" -> return ()
    Just "q" -> return ()
    Just "c0" -> readEvalPrint c0 >> loop
    Just "c1" -> readEvalPrint c1 >> loop
    Just "c2" -> readEvalPrint c2 >> loop
    Just "succ0" -> readEvalPrint (cSucc ++ " " ++ c0) >> loop
    Just "succ1" -> readEvalPrint (cSucc ++ " " ++ c1) >> loop
    Just "plus" -> readEvalPrint (cPlus ++ " " ++ c0 ++ " " ++ c1) >> loop
    Just s -> readEvalPrint s >> loop

readEvalPrint :: MonadIO m => String -> InputT m ()
readEvalPrint input =
  showResult $ C.eval1 <$> P.parseUntypedLambda input
  where
    showResult x =
      case x of
        Left msg -> outputStrLn $ show msg
        Right expr -> outputStrLn $ renderExpression expr

{- Church Encoding for numbers {0, 1, 2} and operations plus and succ -}
c0 :: String
c0 = "(λf.λx.x)"

c1 :: String
c1 = "(λf.λx.f x)"

c2 :: String
c2 = "(λf.λx.f (f x))"

cPlus :: String
cPlus = "(λm.λn.λf.λx.m f (n f x))"

cSucc :: String
cSucc = "(λn.λf.λx.f (n f x))"
