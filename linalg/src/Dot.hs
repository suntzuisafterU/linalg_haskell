module Dot where

import Vec

-- dot product properties:
--   pending: basic properties
--
--   Finding included angle with the dot product
--                u `dot` v  = ||u|| * ||v|| * cos(theta)
--      therefor: cos(theta) = (u `dot` v)/(||u|| * ||v||)
--           and: theta      = arccos(u `dot` v)/(||u|| * ||v||)
--


-- u `dot` v = the sum from i=1 to k of v_subscript_i * u_subscript_i
-- this is a reducing operation, i.e. it reduces the values of a vector
-- to a single value.
dot :: Vec -> Vec -> Float
dot u v = fromRational ( sum (zipWith (*) (coeffs u) (coeffs v)) )

-- ||u|| = sqrt (u `dot` u)
-- equivalently: ||u||**2 = u `dot` u
-- Note that this formula is derived from the Pythagoras Theorem,
-- and is just a special case of the generalized distance formula
-- in n dimensions.  i.e.
-- For any two points in some vector space R^n, say, P and Q,
-- the distance from P to Q is |P - Q|
--    = sqrt( the sum from i=1 to n of |p_sub_i - q_sub_i|**2 )
--    note the absolute value bars in this definition.
-- Then the magnitude function is a special case of this formula where we
-- seek the distance from the origin, (aka the zero vector) to P, in this
-- case every term in the summation simplifies to p_sub_i**2, i.e., we are
-- summing the values of p_sub_i * p_sub_i, which is equivalent to the dot
-- product.  Then take the sqrt and we are done.
mag :: Vec -> Float
mag u = sqrt (u `dot` u)

-- This function is important, but also requires support of floating point numbers in
-- our Matrices and Vectors, unless we come up with a more robust exact value representation
-- that includes sqrt symbols and possibly constents like pi.
-- TODO: Scrap or fix.  Not strictly required for the functionality that I seek.
--
-- docs:
-- normalizing a vector gives a unit vector that is pointed in the same direction as
-- the original value.
-- the unit vector of some vector v in R^n = v * (1/||v||)
normalize :: Vec -> Vec
normalize v = v `mult` (1 / (mag v))

