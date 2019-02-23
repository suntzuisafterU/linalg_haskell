module Dot where

import Frac
import Vec

dot :: Vec -> Vec -> Frac
dot u v = sum (zipWith (*) (getCoeffs u) (getCoeffs v))

mag :: Floating a => Vec -> a
mag u = fsqrt (u `dot` u)

