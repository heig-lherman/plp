-- Solution 1

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

-- Solution 2

unique' :: Eq a => [a] -> [a]
unique' [] = []
unique' [x] = [x]
unique' (x:xs) = x : unique' (remove x xs)
    where
        remove _ [] = []
        remove x (y:ys)
            | x == y = remove x xs 
            | otherwise = y : remove x ys

-- Solution 3

unique'' :: Eq a => [a] -> [a]
unique'' xs = inner xs []
    where
        inner [] ys = reverse ys
        inner (x:xs) seen
            | x `elem` seen = inner xs seen
            | otherwise = inner xs (x:seen)
