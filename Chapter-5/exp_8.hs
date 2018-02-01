positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [t | t <- find x $ zip xs [1..]]

find :: Eq a => a -> [(a, b)] -> [b]
find x xs = [x2 | (x1, x2) <- xs, x1 == x]
