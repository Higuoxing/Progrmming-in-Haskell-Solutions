data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf t) 
  | EQ == Ordering x t = True
  | otherwise          = False
occurs x (Node l v r)
  | LT == Ordering x v = occurs x l
  | GT == Ordering x v = occurs x r
  | otherwise          = True
