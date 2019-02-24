module Mat where

import Vec
import Frac

type MatCoeffs = [[ Frac ]]

getMatCoeffs :: Mat -> MatCoeffs
getMatCoeffs (NormMat c)      = c
getMatCoeffs (AugmentedMat c) = c

data Mat = AugmentedMat MatCoeffs | NormMat MatCoeffs
  deriving (Eq)

instance Show Mat where
  show = showMat

showMat :: Mat -> String
showMat m = foldl (\acc x -> acc ++ (show (RowVec (x))) ++ "\n") "" (getMatCoeffs m)





