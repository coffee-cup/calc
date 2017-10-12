module Eval where

import Syntax

binaryOp :: Expr -> Expr -> (Double -> Double -> Double) -> Maybe Double
binaryOp e1 e2 op = do
  n1 <- eval e1
  n2 <- eval e2
  Just (n1 `op` n2)

eval :: Expr -> Maybe Double
eval x = case x of
  LNum t    -> Just t
  Neg e     -> binaryOp e (LNum (-1)) (*)
  Add e1 e2 -> binaryOp e1 e2 (+)
  Sub e1 e2 -> binaryOp e1 e2 (-)
  Mul e1 e2 -> binaryOp e1 e2 (*)
  Div e1 e2 -> binaryOp e1 e2 (/)
