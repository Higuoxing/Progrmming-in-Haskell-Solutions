import Data.Char

lowers :: String -> Int
lowers [] = 0
lowers (c:cs) | isLower c = 1 + lowers cs
              | otherwise = lowers cs

count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x = 1 + count c xs
               | otherwise = count c xs

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
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
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2/e) | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [t | t <- find x $ zip xs [1..]]

find :: Eq a => a -> [(a, b)] -> [b]
find x xs = [x2 | (x1, x2) <- xs, x1 == x]

crack :: String -> String
crack xs = rotate (-fac) xs
    where fac = head $ find (minimum chitable) (zip chitable [1..])
          chitable = [1..26]
