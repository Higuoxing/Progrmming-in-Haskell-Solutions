-- altMap
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:xs) = (f x) : altMap g f xs

-- Luhn algorithm
luhn :: [Int] -> Bool
luhn [] = False
luhn xs 
    | sumOfNums `mod` 10 == 0 = True
    | otherwise = False
        where sumOfNums = sum $ altMap (\x -> 0) luhnDouble xs

-- luhnDouble
luhnDouble :: Int -> Int
luhnDouble n 
    | n > 4 = 2*n - 9
    | otherwise = 2*n
