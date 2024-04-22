{-
  Utils.hs - Utility functions for CSV handling.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV.Utils
  ( padRight,
    removeAt,
    rstrip,
    split,
    trimmedLines,
  )
where

import Data.Char (isSpace)

-- | Right-pads a string with a given character
padRight :: Char -> Int -> String -> String
padRight c len s = s ++ replicate (len - length s) c

-- | Remove an element from a list by its index
removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt i xs = map snd . filter ((/= i) . fst) $ zip [0 ..] xs

-- | Strip a string from potential trailing whitespaces
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

-- | Split a string on a given delimiter
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

-- | Splits every lines removing potential carriage returns
trimmedLines :: String -> [String]
trimmedLines = map rstrip . lines
