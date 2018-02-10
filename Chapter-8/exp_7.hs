class Eq (Maybe a) where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq a => Eq (Maybe a) where
  Just a  == Just a  = True
  Nothing == Nothing = True
  _       == _       = False

instance Eq a => Eq [a] where
  

