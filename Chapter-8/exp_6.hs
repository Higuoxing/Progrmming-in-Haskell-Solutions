data Expr = Val Int | Add Expr Expr
  deriving Show

-- value
value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

-- folde
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add expr1 expr2) = g (folde f g expr1) (folde f g expr2)

-- eval
eval :: Expr -> Int
eval (Val x) = x
eval (Add expr1 expr2) = eval (expr1) + eval (expr2)

-- size
size :: Expr -> Int
size expr = folde (\x -> 1) (+) expr
