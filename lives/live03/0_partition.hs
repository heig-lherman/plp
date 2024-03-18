-- >>>  partition even [1..10]
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs) = if p x then (x : ys, zs) else (ys, x : zs)
 where
  (ys, zs) = partition p xs

-- >>> partition' even [1..10]
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = go xs [] []
 where
  go [] ys zs = (reverse ys, reverse zs)
  go (x : xs) ys zs
    | p x = go xs (x : ys) zs
    | otherwise = go xs ys (x : zs)
