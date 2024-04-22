{-
  CSV.hs - Main definition for the CSV library module.
  Authors: Loïc Herman, Massimo Stefani
-}
module CSV
  ( -- * Types
    CSV (File),
    Header,
    Record,
    Field,
    Name,

    -- ** Filter expressions
    EqualityFilter (EqFilter),

    -- * Manipulation
    create,

    -- ** String conversion
    format,

    -- ** Sorting
    sortRecords,

    -- ** Filtering
    filterRecords,

    -- ** Aggregating
    combine,
    merge,
    join,
    joinMany,
  )
where

import CSV.Aggregation
import CSV.File
import CSV.Filter
import CSV.Format
import CSV.Sort
