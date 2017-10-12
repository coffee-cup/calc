module Eval where

import Syntax

eval :: Expr -> Maybe Integer
eval x = case x of

  LInt t    -> Just t

  Add e1 e2 -> do
    n1 <- eval e1
    n2 <- eval e2
    Just (n1 + n2)

  Sub e1 e2 -> do
    n1 <- eval e1
    n2 <- eval e2
    Just (n1 - n2)

  Mul e1 e2 -> do
    n1 <- eval e1
    n2 <- eval e2
    Just (n1 * n2)

  Div e1 e2 -> do
    n1 <- eval e1
    n2 <- eval e2
    Just (n1 `div` n2)
