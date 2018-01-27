{-
 file: exp_4.hs
-}

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [s | s <- xs, s <= x]
          larger  = [l | l <- xs, l >  x]

rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
    where larger  = [l | l <- xs, l  > x]
          smaller = [s | s <- xs, s <= x]
