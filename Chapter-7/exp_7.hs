import Data.Char

type Bit = Int

-- bin2int
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w, b) <- zip weights bits]
    where weights = iterate (*2) 1


-- int2bin
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- make8
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- encode'
encode' :: String -> [Bit]
encode' = concat . map (make8 . int2bin . ord)

-- chop8
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = (take 8 bits) : (chop8 . drop 8) bits

-- decode'
decode' :: [Bit] -> String
decode' = map (chr . bin2int) . chop8

-- transmit
transmit :: String -> String
transmit = decode' . channel . encode'

-- channel
channel :: [Bit] -> [Bit]
channel = (\x -> x)

