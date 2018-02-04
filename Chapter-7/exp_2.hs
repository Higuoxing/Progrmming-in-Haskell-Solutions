all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs)
    | p x = all' p xs
    | otherwise = False         

all'' :: (a -> Bool) -> [a] -> Bool
all'' f xs = foldr (&&) True [f x | x <- xs]

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) 
    | p x = True
    | otherwise = any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x : xs
