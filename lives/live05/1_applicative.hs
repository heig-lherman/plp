-- >>> prods [2,3] [4,5]
-- [8,10,12,15]
prods :: [Int] -> [Int] -> [Int]
prods xs ys = (*) <$> xs <*> ys
