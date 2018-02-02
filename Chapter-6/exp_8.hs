halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take hf xs, drop hf xs)
    where hf = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x > y  = y : merge ys (x:xs)
    | y >= x = x : merge xs (y:ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge x1 x2
    where 
        x1 = msort xs1
        x2 = msort xs2
        (xs1, xs2) = halve xs
