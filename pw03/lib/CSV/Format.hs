{-
  Format.hs - Helpers for displaying CSV files in text table form.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV.Format
  ( format,
  )
where

import CSV.File (CSV (File), Record)
import CSV.Utils (padRight)

-- | Computes the maximum width of each column.
columnWidths :: CSV -> [Int]
columnWidths (File header records) = foldr (zipWith max . map length) (map length header) records

-- | Takes a list of column widths and a record and formats it as string.
showRecord :: [Int] -> Record -> String
showRecord sizes file = unwords $ zipWith (padRight ' ') sizes file

-- | Format a CSV aligning each column.
format :: CSV -> String
format csv@(File header records) =
  let widths = columnWidths csv
   in unlines $ map (showRecord widths) (header : records)
