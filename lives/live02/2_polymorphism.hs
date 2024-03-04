sorted :: Ord a => [a] -> Bool
sorted (x:y:zs) = x <= y && sorted (y:zs)
sorted _ = True

