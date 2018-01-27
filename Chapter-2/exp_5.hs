{-
 file: exp_5.hs
-}

init' []     = error "No result for empty list"
init' (x:[]) = []
init' (x:xs) = [x] ++ init' xs

init'' []    = error "No result for empty list"
init'' xs    = take (length xs - 1) xs
