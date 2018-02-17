data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> f a -> f b
  fmap g Nil = Nil
  fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)
