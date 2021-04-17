{-# LANGUAGE LambdaCase #-}

module LambdaData.Data where

import           Prettyprinter

data Expr
  = Var Int
  | Abs Expr
  | App Expr Expr
  | Named String
  | None
  deriving (Show, Eq)

instance Pretty Expr where
  pretty = \case
    Var   num          -> pretty $ "#" <> show num
    Abs   body         -> pretty "λ." <> pretty body
    App   first second -> parens (pretty first) <> parens (pretty second)
    Named str          -> pretty str
    None               -> mempty
