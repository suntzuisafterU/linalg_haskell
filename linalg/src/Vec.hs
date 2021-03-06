module Vec where
    -- ( module Frac
    -- ) where

import Frac

-- TODO: implement a typeclass for Vectors and Matrices together, then one for each separately
-- class

-- type synonym for concise
type Coeffs = [ Frac ]
type FCoeffs = [ Float ]

data Vec = ColumnVec Coeffs | RowVec Coeffs
  deriving (Eq)

data FloatingVec = FColumnVec FCoeffs | FRowVec FCoeffs

class ScalarOps a where
  mult :: a -> Frac -> a

instance ScalarOps Vec where
  mult (RowVec u) x = (RowVec (map (*x) u))
  mult (ColumnVec u) x = (ColumnVec (map (*x) u))

-- Retrieve the Coeffs for calculations
getCoeffs :: Vec -> Coeffs
getCoeffs (ColumnVec u) = u
getCoeffs (RowVec u)    = u

instance Show Vec where
  show = showVec

showVec :: Vec -> String
showVec (ColumnVec x) = "[\n" ++ dispColVec (ColumnVec x)
showVec (RowVec x)    = "[" ++ foldr (\y accum -> " " ++ (show y) ++ accum) " ]" x -- Lambda function to foldr to reduce RowVec

-- Pattern matching to recursively display ColumnVec
dispColVec :: Vec -> String
dispColVec (ColumnVec []) = "  ]\n"
dispColVec (ColumnVec (x:xs)) = " " ++ (show x) ++ "\n" ++ (dispColVec (ColumnVec xs))

-- Vector addition
add :: Vec -> Vec -> Vec
add (ColumnVec u) (ColumnVec v) = (ColumnVec (zipWith (+) u v))
add (RowVec u) (RowVec v) = (RowVec (zipWith (+) u v))


-- Record syntax, auto generates functions to lookup fields of the record.
-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      } deriving (Show)
