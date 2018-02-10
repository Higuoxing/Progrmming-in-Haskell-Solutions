data Expr = Val Int | Add Expr Expr | Mult Expr Expr
  deriving Show

-- evaluate
eval :: Expr -> Int
eval (Val x) = x
eval (Mult x y) = (eval x) * (eval y)
eval (Add x  y) = eval x + eval y
