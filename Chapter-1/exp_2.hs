{-
 file: exp_2.hs
-}

sum' []     = 0
sum' (x:xs) = x + sum' xs
