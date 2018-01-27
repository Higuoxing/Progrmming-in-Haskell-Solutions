{-
 file: exp_4.hs
-}

last' []     = error "No result for empty list"
last' (x:[]) = x
last' (x:xs) = last' xs
