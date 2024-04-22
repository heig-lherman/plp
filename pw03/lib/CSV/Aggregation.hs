{-
  Aggregation.hs - Toolchains to aggregate CSV data, by combining, merging and joining.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV.Aggregation
  ( combine,
    merge,
    join,
    joinMany,
  )
where

import CSV.File (CSV (File), Name)
import CSV.Utils (removeAt)
import Data.List (elemIndex)

-- | Merge two CSV files, throws an error if headers don't match.
combine :: CSV -> CSV -> CSV
combine (File h1 r1) (File h2 r2)
  | h1 == h2 = File h1 (r1 ++ r2)
  | otherwise = error "Headers don't match"

-- | Merge multiple CSV files into one.
merge :: [CSV] -> CSV
merge [] = error "No CSV files to merge"
merge [csv] = csv
merge (csv : csvs) = foldl combine csv csvs

-- | Join two CSV files on a common column.
join :: Name -> CSV -> CSV -> CSV
join column (File h1 rs1) (File h2 rs2) =
  case (elemIndex column h1, elemIndex column h2) of
    (Just i1, Just i2) -> File mergedHeaders (joinRecords i1 i2)
    _ -> File mergedHeaders []
  where
    mergedHeaders = h1 ++ filter (/= column) h2
    joinRecords i1 i2 = [r1 ++ removeAt i2 r2 | r1 <- rs1, r2 <- rs2, r1 !! i1 == r2 !! i2]

-- | Join multiple CSV files on a common column.
joinMany :: Name -> [CSV] -> CSV
joinMany _ [] = error "No CSV files to join"
joinMany column [csv@(File h _)] = if column `elem` h then csv else File h []
joinMany column (csv : csvs) = foldl (join column) csv csvs
