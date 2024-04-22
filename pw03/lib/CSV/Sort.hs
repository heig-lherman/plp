{-
  Sort.hs - Tools to sort CSV files by a column.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV.Sort
  ( sortRecords,
  )
where

import CSV.File (CSV (File), Name, Record)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)

-- | Compare records by a list of indices
compareByIndices :: [Int] -> Record -> Record -> Ordering
compareByIndices [] _ _ = EQ
compareByIndices (i : is) r1 r2 =
  case compare (r1 !! i) (r2 !! i) of
    EQ -> compareByIndices is r1 r2
    x -> x

-- | Sorts a CSV file by the given columns in ascending order.
-- | If a column is not found, an error is raised.
sortRecords :: [Name] -> CSV -> CSV
sortRecords columns (File header records) =
  let indices = map (\col -> fromMaybe (error ("Column " ++ col ++ " not found")) (elemIndex col header)) columns
   in File header (sortBy (compareByIndices indices) records)
