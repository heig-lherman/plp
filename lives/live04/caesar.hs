import Data.Char
import System.Environment (getArgs, getProgName)

-- | Encode a character using Caesar cipher
-- >>> encodeChar 3 'a'
-- 'd'
encodeChar :: Int -> Char -> Char
encodeChar shift c = chr $ (ord c - base + shift) `mod` 26 + base
  where
    base = if isLower c then ord 'a' else ord 'A'

-- | Applies the caesar cipher to a string, should not encode characters
--   that are not letters.
-- >>> caesarCipher 3 "Hello, world!"
-- "Khoor, zruog!"
--
-- >>> caesarCipher (-3) "Khoor, zruog!"
-- "Hello, world!"
caesarCipher :: Int -> String -> String
caesarCipher shift str = [if isAlpha c then encodeChar shift c else c | c <- str]

-- | Display usage information
-- >>> displayUsage
-- Usage: 0_encoding <inputFile> <outputFile> <shift>.
displayUsage :: IO ()
displayUsage = do
  prog <- getProgName
  putStrLn $ "Usage: " ++ prog ++ " <inputFile> <outputFile> <shift>."

-- | Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile, shiftStr] -> do
      input <- readFile inputFile
      let shift = read shiftStr :: Int
      let output = caesarCipher shift input
      writeFile outputFile output
    _ -> displayUsage