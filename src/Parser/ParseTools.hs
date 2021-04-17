{-# LANGUAGE LambdaCase #-}

module Parser.ParseTools where

import LambdaData.Data
import Parser.ProgramTypes

-- TODO Les mettre en dehors de ce fichier
recurInt :: a -> (Int -> a -> a) -> Int -> a
recurInt a _ 0 = a
recurInt a f other
  | other < 0
  = a
  | otherwise
  = let predOther = pred other in f predOther $ recurInt a f predOther

add1Comp :: Int -> Expr -> Expr
add1Comp _ (Abs (Abs val)) = Abs $ Abs $ App (Var 2) val
add1Comp _ _               = undefined


foundMain :: [FunctionDec] -> Program
foundMain list = case foldl putMainHead [] list of
  ((FunctionDec "main" value):rest) -> Program value rest
  other                             -> Program None  other

putMainHead ::[FunctionDec] -> FunctionDec -> [FunctionDec]
putMainHead acc = \case
  a@(FunctionDec "main" _) -> a : acc
  rest                     -> acc ++ [rest]
