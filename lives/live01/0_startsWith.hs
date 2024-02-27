-- Solution 1

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith xs ys = take (length xs) ys == xs

-- Solution 2

startsWith' :: Eq a => [a] -> [a] -> Bool
startsWith' [] _          = True
startsWith' _ []          = False
startsWith' (x:xs) (y:ys) = x == y && startsWith xs ys
