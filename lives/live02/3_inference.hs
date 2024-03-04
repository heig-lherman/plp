f :: (Eq a) => a -> [a] -> [[a]]
f x ys = g ys [] []
  where
    g [] r t = reverse (r:t)
    g (y:z) r t
      | y == x    = g z [] (r:t)
      | otherwise = g z (r ++ [y]) t

