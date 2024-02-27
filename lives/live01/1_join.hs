join :: String -> [String] -> String
join sep [] = ""
join sep [x] = x
join sep (x:xs) = x ++ sep ++ join sep xs
