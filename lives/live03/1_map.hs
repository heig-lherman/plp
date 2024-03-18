-- >>> map (+1) [1..5] id
map' :: (a -> b) -> [a] -> ([b] -> c) -> c
map' _ [] k = k []
map' f (x : xs) k = map' f xs (\ys -> k (f x : ys))
