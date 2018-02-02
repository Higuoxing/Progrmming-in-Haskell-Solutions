import Data.Char

upperOrLowers :: String -> Int
upperOrLowers [] = 0
upperOrLowers (c:cs) | (isLower c || isUpper c) = 1 + upperOrLowers cs
                     | otherwise = upperOrLowers cs

count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x = 1 + count c xs
               | otherwise = count c xs

let2int :: Char -> Int
let2int c | isUpper c = ord c - ord 'A'
          | isLower c = ord c - ord 'a'
          | otherwise = 0

shift :: Int -> Char -> Char
shift n c | isUpper c = chr ((let2int c + n) `mod` 26 + ord 'A')
          | isLower c = chr ((let2int c + n) `mod` 26 + ord 'a')
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n s | s <- xs]

table :: [Float]
table = [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
          0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
          6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x $ toLowers xs) n | x <- ['a'..'z']]
    where n = upperOrLowers xs

toLowers :: String -> String
toLowers xs = [toLower s | s <- xs]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2/e) | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [t | t <- find x $ zip xs [1..]]

find :: Eq a => a -> [(a, b)] -> [b]
find x xs = [x2 | (x1, x2) <- xs, x1 == x]

crack :: String -> String
crack xs = encode (-factor+1) xs
    where
        factor = head (positions' (minimum chitab) chitab) 
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

