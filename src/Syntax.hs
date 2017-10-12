module Syntax where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | Neg Expr
  | LNum Double
  deriving (Eq, Show)