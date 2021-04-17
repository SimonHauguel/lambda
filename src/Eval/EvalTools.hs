{-# LANGUAGE LambdaCase #-}

module Eval.EvalTools where


import           LambdaData.Data                ( Expr(..) )
import           Parser.ProgramTypes
import           Data.List


betaRed :: [FunctionDec] -> Expr -> Expr
betaRed values (App fun arg) = case betaRed values fun of
  Abs body    -> betaRed values $ inject 1 body arg
  a@(App _ _) -> betaRed values $ App a arg
  other       -> App other arg
betaRed values (Named name)  = betaRed values $ values `get` name
betaRed _ x = x


get :: [FunctionDec] -> String -> Expr
get values name = case find (\(FunctionDec sec _) -> name == sec) values of
  Just (FunctionDec _ val) -> val
  _                        -> undefined


inject :: Int -> Expr -> Expr -> Expr
inject num body arg = case body of
  val@(Var a)
    | a == num     -> arg
    | otherwise    -> val
  Abs a            -> Abs $ inject (succ num) a arg
  App first second -> App (inject num first arg) (inject num second arg)
  a@(Named _)      -> a
  _                -> undefined

toLambda :: [FunctionDec] -> Expr -> Expr
toLambda values = \case
  Abs lam     -> Abs $ toLambda values lam
  App fir sec -> App (toLambda values fir) (toLambda values sec)
  Named name  -> toLambda values $ values `get` name
  other       -> other
