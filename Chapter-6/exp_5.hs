-- length
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- drop
drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (x:xs) = [] ++ drop' (n-1) xs

-- init
init' :: [a] -> [a]
init' [] = []
init' (x:[]) = []
init' (x:xs) = x: init' xs
