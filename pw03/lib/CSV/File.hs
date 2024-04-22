{-
  File.hs - Basic types for representing CSV files.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV.File
  ( CSV (File),
    Header,
    Record,
    Name,
    Field,
    create,
  )
where

import CSV.Utils (split, trimmedLines)
import Data.List (intercalate)

-- | A CSV file consists of one header and a list of associated records.
data CSV
  = File Header [Record]
  deriving (Eq)

-- | Showing the CSV file converts it back to the standard file format.
instance Show CSV where
  show (File header records) =
    unlines $ map (intercalate [delimiter]) (header : records)

-- | Reading a CSV file parses the standard file format to the internal representation.
instance Read CSV where
  readsPrec _ input = [(create input, "")]

-- | The header simply resolves to a list of column names.
type Header = [Name]

-- | The name is a simple string representing a column name.
type Name = String

-- | The record is a list of fields representing a row in the CSV file.
type Record = [Field]

-- | The field is a simple value for one cell in a Record row.
type Field = String

-- | The delimited to use while parsing.
delimiter :: Char
delimiter = ','

-- | Construct a CSV from a multiline string.
create :: String -> CSV
create input = case trimmedLines input of
  [] -> error "The given CSV file is empty."
  (header : rows) -> File (split delimiter header) (map (split delimiter) rows)
