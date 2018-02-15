fmap0 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
fmap0 _ _ Nothing = Nothing
fmap0 _ Nothing _ = Nothing
fmap0 f (Just a) (Just b) = Just (f a b)
