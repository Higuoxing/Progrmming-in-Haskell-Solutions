{-
 file: exp_1.hs
-}

halve :: [a] -> ([a], [a])
halve xs        = (take halfl xs, drop halfl xs)
    where halfl = length xs `div` 2
