module Dot where

import Frac
import Vec

dot :: Vec -> Vec -> Float
dot u v = toFloat ( sum (zipWith (*) (getCoeffs u) (getCoeffs v)) )

mag :: Vec -> Float
mag u = sqrt (u `dot` u)

