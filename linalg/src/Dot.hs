module Dot where

import Vec

-- dot product properties:
--   pending: basic properties
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

-- The included angle between two vectors is calculated by:
--              u `dot` v  = ||u|| * ||v|| * cos(theta)
--    therefor: cos(theta) = (u `dot` v)/(||u|| * ||v||)
--         and: theta      = arccos(u `dot` v)/(||u|| * ||v||)
--
-- includedAngle :: Vec -> Vec -> Float


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


-- Two vectors are perpendicular if:
--   1. They are both in the same vector space.
--   2. Neither is the zero vector.
--   3. Their dot product is 0.
-- arePerpendicular :: Vec -> Vec -> Bool -- TODO: should this throw an error if we get a zero vector? As opposed to just reporting false?
-- arePerpendicular u v = (not.isZeroVec u) && (not.isZeroVec v) && (u `dot` v) == 0

-- The projection of a vector v onto the vector u is defined by a vector that is a scalar product of u and whose point is positioned so that
-- a vector from that point to v would be perpendicular to v.  These 2 new vectors together form a right angle triangle.
-- The formula for the projection of v onto u is:
--       proj(v -> u) = k*u, where k = ((v `dot` u)/||u||^2)
-- note that the form k*u informs us that the resulting vector is in the same direction as u.
-- The presence of ||u||^2 in the denominator of k means that as u gets longer, the projection gets shorter.
-- Also, since v is only present in the denominator, and only influences k, the closer the two vectors are to pointing in the same direction,
-- the larger the value of the projection.  The projection approaches the exact value of v as u and v approach pointing in the same direction.
--
-- projection :: Vec -> Vec -> Vec
-- projection u v = ((u `dot` v)/(mag u)**2) `mult` u
