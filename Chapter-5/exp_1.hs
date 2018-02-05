sumOfSquare :: Int -> Int
sumOfSquare n | n <= 0    = 0
              | otherwise = foldr (\x1 x2 -> x1^2 + x2) 0 [1..n]

