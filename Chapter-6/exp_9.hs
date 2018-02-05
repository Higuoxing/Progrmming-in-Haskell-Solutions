-- sum
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- take
take' :: Int -> [a] -> [a]
take' n []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs

-- last
last' :: [a] -> a
last' []     = error "Empty list"
last' (x:[]) = x
last' (x:xs) = last' xs
