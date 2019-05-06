module SysOfEqn where



-- Show a system of equations as a set of column vectors.
-- This corresponds to x1 * [a b c]T + x2 * [d e f]T ... + xn * [x y z]T = [A B C]T
--                       where the final vector of constants is optional, and if omitted
--                       is assumed to be the zero vector, corresponding with a
--                       homogenous system of equations.
-- showAsVectors :: Sys -> IO()

-- The basic solution of a system are columns constructed from the coefficients on parameters in the solution.
-- The basic solutions are often represented by X₁, X₂, etc.
-- NOTE: on page 29, example 1.30 of text.  TODO: Remove references to text
-- showBasicSolution :: Sys -> IO()

-- show

-- A systen of equations is homogenous when all of its formulas are set equal to zero.
-- This corresponds to the equation AX = B where
--   A = coefficient matrix
--   X = variable column vector, [x1 x2 ... xn]T
--   B = the column vector of constants, and is the zero vector in n, i.e.
--       [0 0 ... 0]T, with n components
-- isHomogenous :: Sys -> Bool

-- Homogenous systems by definition have the trivial solution.
-- The trivial vector is the zero vector.  Therefor if a system of equations has the trivial solution
-- it means that the zero vector is a solution to the values of its variables.
-- i,e, [x1 x2 ... xn] = the zero vector
-- hasTrivialSolution :: Sys -> Bool
-- hasTrivialSolution = isHomogenous

-- The important question to ask about homogenous systems of equations is if they have a non-trivial solution.
-- We know that all homogenous systems have at least one solution, the trivial one.
-- If they have any other solutions, then there are many conclusions that can be drawn from this information.
-- Equivalently??: If a homogenous system of m equations in n variables has m < n then it will always have a
--                 non-trivial solution.
-- hasNonTrivialSolution :: Sys -> Bool
-- hasNonTrivialSolution = TODO: implement

-- A system of equations is linearlly independent if the ONLY solution is the trivial solution.
--   i,e, the vector equation:  x1v1 + ... + xnvn = 0 has ONLY the trivial solution.
-- This also corresponds to a system whose coefficient matrix in RREF is the identity matrix I^n
-- TODO: what if it is heterogenous and has a unique solution?  Then the whole vector space could be shifted to the origin and bang, heterogenous
--
-- isLinearlyIndependent :: Sys -> Bool

-- A homogenous system of equations is linearly dependent if it has at least one non-trivial solution.
-- isLinearlyDependent :: Sys -> Bool
-- isLinearlyDependent = hasMultipleSolutions?? -- or, not.isLinearlyIndependent -- or, not.hasUniqueSolution


-- A system of equations is heterogenous when any number of its formulas are not set to zero.
-- i,e, in the formula AX = B (see above), the column vector of n constants B has at least one
--      none zero value
-- isHeterogenous :: Sys -> Bool

-- If the vector equation x1v1 + x2v2 + ... + xnvn = b has at least one solution, then it is consistent.
--   note: if this is true we say that b is a linear combination of v1,v2,...,vn
-- Since homogenous systems always have the trivial solution, we consider consistence mainly for heterogenous systems.
-- A heterogenous system without a row of zeroes will ALWAYS be consistent.
-- isConsistent :: Sys -> Bool
-- isConsistent s = (isHomogenouse) || (not.hasRowOfZeroes -- with a non-zero augmented value)

-- A system of equations is inconsistent if it is heterogenous (and therefor does not have the trivial solution)
-- and it also does not have the non-trivial solution.
--
-- Geometrically this means that the equations associated with this system define a set of planes in R^(m<=n-1) that never intersect.
-- This means that the equations could define a set of plans that are each lower dimensional than n, and could be so to different degrees.
--
-- This can be shown by having a constant term that is non-zero associated with an equation that has all zero coefficients.
-- That is effectively saying that the sum of some variables multiplied by zero is non-zero, which is not satisfiable.
-- isInconsistent :: Sys -> Bool
-- isInconsistent s = (isHeterogenous s) && (not.hasSolution)

-- There are many ways to define this property.
-- 1. If the system is homogenous, does it have a non-trivial solution?
-- 2. If the system is heterogenous, AND consistent, then does the corresponding homogeneous system have a non-trivial solution?
-- hasManySolutions :: Sys -> Bool
-- hasManySolutions s = ((isHomogenous s) && (hasNonTrivialSolution s)) || ((isHeterogenous s) && (helper s))
--                         where helper = ... -- TODO: implement multi-solution function.
--                                        Technique: 1. Reduce to RREF.
--                                                   2. Check if it has ANY solutions (is consistent)
--                                                   3. Check if the Rank is not equal to either the number of rows or the number of columns.
--                                                      - if this last check fails, then the system has multiple solutions.

-- A system of equation has a unique solution if and only if it is consistent and the columns of the corresponding matrix
-- are linearly independent vectors.
-- Equivalently: A system of equations has a unique solution if the corresponding homogenous system has only the trivial solution.
-- hasUniqueSolution :: Sys -> Bool
-- hasUniqueSolution s = ((isHomogenous s) && (not.hasNonTrivialSolution s)) || ((isHeterogenous s) && (not.hasManySolutions))




------------------ Utility functions -------------------------------
-- hasRowOfZeroes :: Sys -> Bool
-- hasRowOfZeroes = -- fold or map or filter a system and return a boolean value if a row of zeroes is found

-- toHomogenousMat :: Sys -> Mat

-- toAugmentedMat :: Sys -> Mat

