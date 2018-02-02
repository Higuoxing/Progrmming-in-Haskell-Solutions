merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x > y  = y : merge ys (x:xs)
    | y >= x = x : merge xs (y:ys)
