{-
  Filter.hs - Helpers for filtering CSV files.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV.Filter
  ( EqualityFilter (EqFilter),
    parseFilter,
    filterRecords,
  )
where

import CSV.File (CSV (File))
import Data.List (elemIndex)

-- | An equality filter is a column name and a value to compare against.
newtype EqualityFilter = EqFilter (String, String)
  deriving (Show, Eq)

-- | Read an equality filter from a string.
instance Read EqualityFilter where
  readsPrec _ input = case parseFilter input of
    Just filterExpr -> [(filterExpr, "")]
    Nothing -> error "Unrecognized filter expression"

-- | Parse a string into a filter expression.
parseFilter :: String -> Maybe EqualityFilter
parseFilter input =
  case break (== '=') input of
    (column, '=' : value) -> Just (EqFilter (column, value))
    _ -> Nothing

-- | Filter records based on a list of equality filters.
-- | If a filter contains a column that is not present in the CSV, the result will be empty
filterRecords :: [EqualityFilter] -> CSV -> CSV
filterRecords filters (File header records) =
  case traverse getFilterIndex filters of
    Nothing -> File header []
    Just indices ->
      let applyFilter record = all (\(idx, val) -> record !! idx == val) indices
       in File header (filter applyFilter records)
  where
    getFilterIndex (EqFilter (col, val)) = fmap (,val) (elemIndex col header)
