{-
 file: exp_3.hs
-}

-- conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then []
              else tail xs

-- guarded equations
safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

-- pattern matching
safetail'' :: [a] -> [a]
safetail'' xs = drop 1 xs
