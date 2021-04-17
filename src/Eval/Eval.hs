module Eval.Eval where


import           Parser.LambdaParser
import           Parser.ProgramTypes
import           Eval.EvalTools
import           Prettyprinter
import           Text.Parsec


eval :: IO ()
eval = do
  contents <- readFile "./src/Eval/some.lam"
  case parse program "An error occured during parsing" contents of
    Right (Program body values) -> print $ pretty $ toLambda values $ betaRed values body
    Left err                    -> print err
