module Parser.ProgramTypes where

import LambdaData.Data

data Program =
  Program Expr [FunctionDec]
  deriving Show


data FunctionDec =
  FunctionDec String Expr
  deriving Show
