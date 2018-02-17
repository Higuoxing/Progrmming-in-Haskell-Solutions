newtype Ntype a b = N (a -> b)

instance Functor (Ntype a) where
  fmap f (N g) = N (f . g)
