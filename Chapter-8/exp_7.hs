{-
instance Eq a => Eq (Maybe a) where
  Just a == Just b = a == b
  Nothing == Nothing = True
  _       == _       = False
-}

{-
instance Eq a => Eq [a] where
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
  []     == []     = True
  _      == _      = False
-}
