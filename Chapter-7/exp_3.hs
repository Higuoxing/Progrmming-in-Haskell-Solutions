mapf :: (a -> b) -> [a] -> [b]
mapf f xs = foldr (\x y -> (f x) : y) [] xs

filterp :: (a -> Bool) -> [a] -> [a]
filterp p xs = foldr (\x y -> (if p x then x : y else y)) [] xs
