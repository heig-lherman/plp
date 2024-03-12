{-
wc.hs - A simple implementation of the Unix wc command
Authors: Loïc Herman, Massimo Stefani
-}

import System.Environment ( getArgs, getProgName )
import Text.Printf ( printf )
import Control.Monad ( forM_, when, (>=>) )

-- | Count the lines, words, and bytes in a string
count :: String -> (Int, Int, Int)
count xs = (lineCount xs, wordCount xs, byteCount xs)
  where
    lineCount [] = 0
    lineCount (x:xs)
      | x == '\n' = 1 + lineCount xs
      | otherwise = lineCount xs
    
    wordCount [] = 0
    wordCount x = wordCount' x False 0

    wordCount' [] _ n = n
    wordCount' (x:xs) inWord n
      | isSpace x && inWord = wordCount' xs False n
      | not (isSpace x) && not inWord = wordCount' xs True (n + 1)
      | otherwise = wordCount' xs inWord n

    isSpace x = x `elem` [' ', '\t', '\n'] 
      
    byteCount [] = 0
    byteCount (x:xs) = 1 + byteCount xs

-- | Compute the total of a list of counts
total :: [(Int, Int, Int)] -> (Int, Int, Int)
total counts = add counts (0, 0, 0)
  where
    add [] a = a
    add ((l', w', b') : counts) (l, w, b) = add counts (l + l', w + w', b + b')


-- | Compute the maximum number of digits in a list of counts
maxDigits :: [(Int, Int, Int)] -> Int
maxDigits counts = max [max [digitCount l, digitCount w, digitCount b] | (l, w, b) <- counts]
  where
    digitCount 0 = 0
    digitCount n = 1 + digitCount (n `div` 10)

    max [] = 0
    max [x] = x
    max (x : x' : xs)
      | x > x' = max (x : xs)
      | otherwise = max (x' : xs)

-- | Display a count
display :: Int -> (Int, Int, Int) -> String -> IO ()
display p (lines, words, bytes) = printf format lines words bytes
  where format = "%" ++ show p ++ "d %" ++ show p ++ "d %" ++ show p ++ "d %s\n"

-- | Print usage information
usage :: IO ()
usage = do
  program <- getProgName
  printf "usage: %s <file> ...\n" program

-- | Main program
main :: IO ()
main = do
  args <- getArgs
  if null args then usage
  else do
    counts <- mapM (readFile >=> (return . count)) args
    let padd = max 5 (maxDigits counts)
    forM_ (zip counts args) $ uncurry (display padd)
    when (length args > 1) (display padd (total counts) "total")
