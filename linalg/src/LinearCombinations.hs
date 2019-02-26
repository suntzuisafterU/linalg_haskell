module LinearCombinations where

import Vec
import Mat
import SysOfEqn

-- The span of a set of vectors is defined by the TODO: finish defining span
-- span :: [Vec] -> ?? TODO: Is the span here represented as a vector space R^n?  Where n <= size of any of the vectors?

-- Given a vector u and a set of vectors V all in R^n, u is a linear combination of some subset of V
-- if it is in the span of V
-- isLinearCombinationOf :: Vec -> [ Vec ] -> (Bool, Maybe Vec) -- TODO: Is Maybe Vec a valid type?

-- containsInternalLinearCombination :: Num a => Mat -> (Bool, Maybe (Vec, [(a, Vec)]))

