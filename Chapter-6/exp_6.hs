-- and
and' :: [Bool] -> Bool
and' []                  = True
and' (b:bs) | b == False = False
            | otherwise  = b && and' bs

-- concat
concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

-- replecate
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate (n - 1) a

-- select
(!!!) :: [a] -> Int -> a
(!!!) [] n = error "Index too large"
(!!!) (x:xs) n | n == 0    = x
               | otherwise = (!!!) xs (n - 1)

-- elem
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x    = True
               | otherwise = elem' a xs
