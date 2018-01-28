{-
 file: exp_2.hs
-}

-- using head and tail
third :: [a] -> a
third xs | length xs < 3 = error "list must have 3 elements at least"
         | otherwise = head $ tail $ tail xs

-- using !!
third' :: [a] -> a
third' xs | length xs < 3 = error "list must have 3 elements at least"
          | otherwise = xs !! 3

-- using pattern matching
third'' :: [a] -> a
third'' xs | length xs < 3 = error "list must have 3 elements at least"
           | otherwise = x
                where (_:_:x:_) = xs
