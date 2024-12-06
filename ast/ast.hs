module Ast where

-- Define the Expr data type
data Expr = Plus Expr Expr 
          | Minus Expr Expr 
          | Times Expr Expr 
          | Div Expr Expr
          | Literal Float

-- Define the eval function
eval :: Expr -> Float
eval (Literal x) = x
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2

-- Test cases (add these to ast.hs)
test1 :: Expr
test1 = Plus (Literal 3.0) (Literal 2.0) -- Should evaluate to 5.0

test2 :: Expr
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0)) -- Should evaluate to 3.5

test3 :: Expr
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0)) -- Should evaluate to 15.5
