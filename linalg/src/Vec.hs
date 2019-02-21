module Vec where
    -- ( module Frac
    -- ) where

import           Frac

data Vec = ColumnVec [ Frac ] | RowVec [ Frac ]
  deriving (Eq)

instance Show Vec where
  show = showVec

showVec :: Vec -> String
showVec (ColumnVec x) = "[\n" ++ dispColVec (ColumnVec x)
showVec (RowVec x)    = "[" ++ foldr (\y accum -> " " ++ (show y) ++ accum) " ]" x -- Lambda function to foldr to reduce RowVec

-- Pattern matching to recursively display ColumnVec
dispColVec :: Vec -> String
dispColVec (ColumnVec []) = "  ]\n"
dispColVec (ColumnVec (x:xs)) = " " ++ (show x) ++ "\n" ++ (dispColVec (ColumnVec xs))



-- Record syntax, auto generates functions to lookup fields of the record.
-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      } deriving (Show)
