func :: (a -> b) -> (a -> Bool) -> [a] -> [b]
func f p xs = [f x | x <- xs, p x]

func' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
func' f p = (\xs -> map f $ filter p xs)
