module Lib
  ( outputMatrix
  ) where

data Matrix = [Fractional] | [[Fractional]]
-- TODO: Instance definitions for show??

outputMatrix :: Matrix->IO()
outputMatrix m   = print m

--TODO: Will the following definition work now?
--formatMatrix :: Matrix->String
--formatMatrix [] = []
--formatMatrix (x:xs) = x:(formatMatrix xs)

