import Prelude(Num, Int, take)

repeat :: a -> [a]
repeat x = x:repeat x

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)
