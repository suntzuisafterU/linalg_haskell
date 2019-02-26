module Mat where

import Vec

type MatCoeffs = [[ Rational ]]

getMatCoeffs :: Mat -> MatCoeffs
getMatCoeffs (NormMat c) = c
getMatCoeffs (AugMat c)  = c

data Mat = AugMat MatCoeffs | NormMat MatCoeffs
  deriving (Eq)

instance Show Mat where
  show = showMat

showMat :: Mat -> String
showMat (NormMat m) = foldl (\acc x -> acc ++ (show (RowVec (x))) ++ "\n") "" m
showMat (AugMat m) = foldl (\acc x -> acc ++ (showAugRow (RowVec (x))) ++ "\n") "" m
  where showAugRow (RowVec x) = "[" ++ (foldr (\y accum -> " " ++ (show y) ++ accum) "" (init x)) ++ " | " ++ (show (last x)) ++ " ]"


-- A matrix is in REF if:
--   1. All of its non-zero rows are above any rows of only zeroes.
--   2. All of its pivots (leading ones) have zeroes below them.
--   3. All of its pivots are strictly below and to the right of any pivot in a row above them.
-- isRowEchelonForm :: Mat -> Bool
-- TODO: implement

-- RREF is a stricter constraint than REF, that is, if a matrix is in
-- RREF than it is also in REF.  The only difference is the condition on having zeroes above pivots.
--
-- A matrix is in RREF if:
--   1. All of its non-zero rows are above any rows of only zeroes.
--   2. All of its pivots are in columns with only zeroes above AND below them.
--   3. All of its pivots are strictly below and to the right of any pivot in a row above them.
-- Note that the identity matrix I^n is in RREF.
-- isReducedRowEchelonForm :: Mat -> Bool
-- TODO: implement


-- If the matrix in RREF has any non-pivot columns, then each non-pivot column represents an arbitrary variable.
-- Consequently, the vectors corresponding to this matrix have a span that includes all of R^n and is also linearly dependent.
-- hasArbitraryVariables :: Mat -> Bool
-- hasArbitraryVariables m = ...
--   where RREFm = if (isReducedRowEchelonForm m) then (m) else (toRREF m)
