{-
 file: exp_8.hs
-}

luhnDouble :: Int -> Int
luhnDouble n | n > 5 = 2 * n - 9
             | otherwise = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | (luhnDouble b + luhnDouble c + luhnDouble d) `mod` 10 == 0 = True
             | otherwise = False
