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
