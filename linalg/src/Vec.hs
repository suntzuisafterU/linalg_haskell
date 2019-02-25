module Vec where
    -- ( module Frac
    -- ) where

import Data.Ratio

-- TODO: implement a typeclass for Vectors and Matrices together, then one for each separately
-- class

data Vec a = ColumnVec [a] | RowVec [a]
  deriving (Eq)

class ScalarOps v where
  multr :: v -> Rational -> v
  mult :: v -> Float -> v

instance ScalarOps (Vec Rational) where
  multr (RowVec u) x    = (RowVec (map (*x) u))
  multr (ColumnVec u) x = (ColumnVec (map (*x) u))

instance ScalarOps (Vec Float) where
  mult (RowVec u) x     = (RowVec (map (*x) u))
  mult (ColumnVec u) x  = (ColumnVec (map (*x) u))

instance Show (Vec a) where
  show = showVec

showVec :: Num a => (Vec a) -> String
showVec (ColumnVec x) = "[\n" ++ dispColVec (ColumnVec x)
showVec (RowVec x)    = "[" ++ foldr (\y accum -> " " ++ (show y) ++ accum) " ]" x -- Lambda function to foldr to reduce RowVec

-- Pattern matching to recursively display ColumnVec
dispColVec :: Num a => (Vec a) -> String
dispColVec (ColumnVec []) = "  ]\n"
dispColVec (ColumnVec (x:xs)) = " " ++ (show x) ++ "\n" ++ (dispColVec (ColumnVec xs))

-- Vector addition
add :: Num a => (Vec a) -> (Vec a) -> (Vec a)
add (ColumnVec u) (ColumnVec v) = (ColumnVec (zipWith (+) u v))
add (RowVec u) (RowVec v) = (RowVec (zipWith (+) u v))


-- Record syntax, auto generates functions to lookup fields of the record.
-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      } deriving (Show)
