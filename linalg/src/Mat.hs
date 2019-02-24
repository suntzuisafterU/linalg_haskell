module Mat where

import Vec
import Frac

type MatCoeffs = [[ Frac ]]

getMatCoeffs :: Mat -> MatCoeffs
getMatCoeffs (NormMat c)      = c
getMatCoeffs (AugMat c) = c

data Mat = AugMat MatCoeffs | NormMat MatCoeffs
  deriving (Eq)

instance Show Mat where
  show = showMat

showMat :: Mat -> String
showMat (NormMat m) = foldl (\acc x -> acc ++ (show (RowVec (x))) ++ "\n") "" m
showMat (AugMat m) = foldl (\acc x -> acc ++ (showAugRow (RowVec (x))) ++ "\n") "" m
  where showAugRow (RowVec x) = "[" ++ (foldr (\y accum -> " " ++ (show y) ++ accum) "" (init x)) ++ " | " ++ (show (last x)) ++ " ]"




