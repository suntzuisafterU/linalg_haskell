module Dot where

import Vec

-- dot product properties:
--   pending: basic properties
--   u `dot` v = ||u|| * ||v|| * cos(theta)
--


dot :: Vec -> Vec -> Float
dot u v = fromRational ( sum (zipWith (*) (coeffs u) (coeffs v)) )

mag :: Vec -> Float
mag u = sqrt (u `dot` u)

normalize :: Vec -> Vec
normalize v = v `mult` (1 / (mag v))

