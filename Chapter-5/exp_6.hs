perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

perfect :: Int -> Bool
perfect n = n == sum'
            where sum' = sum $ factors n

factors :: Int -> [Int]
factors n = [fac | fac <- [1..(n-1)], n `mod` fac == 0]
