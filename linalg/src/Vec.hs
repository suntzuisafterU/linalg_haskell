module Vec where
    -- ( module Frac
    -- ) where

import Data.Ratio

-- TODO: implement a typeclass for Vectors and Matrices together, then one for each separately
-- class

-- type synonym for concise
type Coeffs = [ Rational ]
type FCoeffs = [ Float ]

data Vec = ColumnVec {coeffs :: Coeffs} | RowVec {coeffs :: Coeffs} | FColumnVec {fcoeffs :: FCoeffs} | FRowVec {fcoeffs :: FCoeffs}
  deriving (Eq)

class ScalarOps a where
  multr :: a -> Rational -> a
  mult :: a -> Float -> a

instance ScalarOps Vec where
  multr (RowVec u) x    = (RowVec (map (*x) u))
  multr (ColumnVec u) x = (ColumnVec (map (*x) u))
  mult (RowVec u) x     = (FRowVec (map ((*x).fromRational) u))
  mult (ColumnVec u) x  = (FColumnVec (map ((*x).fromRational) u))

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
add (FColumnVec u) (FColumnVec v) = (FColumnVec (zipWith (+) u v))
add (FRowVec u) (FRowVec v) = (FRowVec (zipWith (+) u v))


-- Record syntax, auto generates functions to lookup fields of the record.
-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      } deriving (Show)
