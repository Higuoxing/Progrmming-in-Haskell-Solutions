import Data.Char

type Bit = Int

-- bin2int
bin2int :: [Bit] -> Int
bin2int bits      = sum [w*b | (w, b) <- zip weights bits]
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

-- addCheckBits
addCheckBits :: [Bit] -> [Bit]
addCheckBits bits = concat $ [addCheckBit x | x <- (chop8 bits)]

-- addCheckBit
addCheckBit :: [Bit] -> [Bit]
addCheckBit bits
    | ((sum bits) `mod` 2 == 0) = bits ++ [0]
    | otherwise                 = bits ++ [1]

-- chop8
chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = (take 8 bits) : (chop8 . drop 8) bits

-- checkParity
checkParity :: [Bit] -> [Bit]
checkParity bits 
    | [] == bits = []
    | otherwise  = (check (take 9 bits)) ++ checkParity (drop 9 bits)

-- check
check :: [Bit] -> [Bit]
check bits
    | ((sum bits) `mod` 2 == 0) = take 8 bits
    | otherwise                 = error "check parity failed"

-- decode'
decode' :: [Bit] -> String
decode' = map (chr . bin2int) . chop8

-- transmit
transmit :: String -> String
transmit = decode' . checkParity . channel . addCheckBits . encode'

-- channel
channel :: [Bit] -> [Bit]
channel = (\x -> x)

