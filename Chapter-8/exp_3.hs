data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

-- size
size :: Tree a -> Int
size (Leaf _)   = 1
size (Node l r) = 1 + max (size l) (size r)

-- balanced
balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r)
  | abs (size l - size r) <= 1 && (balanced l) && (balanced r) = True
  | otherwise = False
