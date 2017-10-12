module Syntax where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | LInt Integer
  deriving (Eq, Show)