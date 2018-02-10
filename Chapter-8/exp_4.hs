-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

-- halve  
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take len xs, drop len xs)
  where len = (length xs `div` 2)

-- construct tree
constructTree :: [a] -> Tree (Maybe a)
constructTree []  = Leaf Nothing
constructTree [a] = Leaf (Just a)
constructTree xs  = Node (constructTree x1) (constructTree x2)
  where (x1, x2) = halve xs
