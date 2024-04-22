{-
  csvkit.hs - Main entry point for the CSV toolkit program.
  Authors: Loïc Herman, Massimo Stefani
-}
module Main where

import CSV
  ( CSV,
    EqualityFilter,
    Name,
    filterRecords,
    format,
    joinMany,
    merge,
    sortRecords,
  )
import System.Environment (getArgs, getProgName)

-- | Main entry point of the program.
main :: IO ()
main = do
  args <- getArgs
  maybe displayUsage runCommand (parseArgs args)

-- | Displays the usage of the program.
displayUsage :: IO ()
displayUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <operation>"
  putStrLn "Operations:"
  putStrLn " -grep   <file> <column>=<value>...  | Filter a CSV according to predicates."
  putStrLn " -format <file>                      | Format a CSV aligning each column."
  putStrLn " -join   <file> <file>... <column>   | Join a CSV with one or more CSVs on a column."
  putStrLn " -merge  <file> <file>...            | Merge two or more CSVs by appending one after another."
  putStrLn " -sort   <file> <column>...          | Sort a CSV by columns in ascending order."

-- | Represents a command to be executed with its required parameters.
data Command
  = Grep FilePath [EqualityFilter]
  | Format FilePath
  | Join [FilePath] Name
  | Merge [FilePath]
  | Sort FilePath [Name]

-- | Parse the given arguments into a command with its parameters.
parseArgs :: [String] -> Maybe Command
parseArgs args = case args of
  ("-grep" : file : conditions) | not $ null conditions -> Just (Grep file (map read conditions))
  ["-format", file] -> Just (Format file)
  ("-join" : files) | length files > 2 -> Just (Join (init files) (last files))
  ("-merge" : files) | length files > 1 -> Just (Merge files)
  ("-sort" : file : columns) | not $ null columns -> Just (Sort file columns)
  _ -> Nothing

-- | Read a CSV file from the filesystem.
parseFile :: FilePath -> IO CSV
parseFile = fmap read . readFile

-- | Run the given command.
runCommand :: Command -> IO ()
runCommand (Grep file conditions) = do
  csv <- parseFile file
  print $ filterRecords conditions csv
runCommand (Format file) = do
  csv <- parseFile file
  putStrLn $ format csv
runCommand (Join files column) = do
  csvs <- mapM parseFile files
  print $ joinMany column csvs
runCommand (Merge files) = do
  csvs <- mapM parseFile files
  print $ merge csvs
runCommand (Sort file columns) = do
  csv <- parseFile file
  print $ sortRecords columns csv
