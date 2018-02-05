euclid :: Int -> Int -> Int
euclid a b | a > b     = euclid (a-b) b
           | a < b     = euclid (b - a) a
           | otherwise = a
