import Data.Char

type Bit = Int

-- unfold
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x 
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

-- chop8
chop8 :: [Bit] -> [[Bit]]
chop8 []        = []
chop8 bits      = take 8 bits : chop8 (drop 8 bits)

-- chop8'
chop8' :: [Bit] -> [[Bit]]
chop8' bits = unfold (== []) (take 8) (drop 8) bits

-- mapf
mapf :: (a -> b) -> [a] -> [b]
mapf f []     = []
mapf f (x:xs) = f x : mapf f xs

-- mapf'
mapf' :: Eq a => (a -> b) -> [a] -> [b]
mapf' f xs = unfold (== []) (f . head) (drop 1) xs

-- iterate'
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- iterate''
iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = unfold (\x -> False) f f x
