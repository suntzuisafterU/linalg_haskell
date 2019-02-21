module Frac where


-- Problem 3 [35 Points]. Declare a data type Frac— for handling fractions -- where each number is a (numerator, denominator) pair of integers representing the numerator and the denominator of the fraction. For example, (22, 23) would represent 22 / 23.  The numerator can be positive or negative, and the denominator must always be positive.

data Frac = Frac (Integer, Integer)

instance Show Frac where
  show = showFrac

showFrac :: Frac -> String
showFrac x
  | denominator x == 1  = show (numerator x)
  | otherwise           = "(" ++ show (numerator x) ++ "/" ++ show (denominator x) ++ ")"


numerator :: Frac -> Integer
numerator (Frac (n, _)) = n

denominator :: Frac -> Integer
denominator (Frac (_, d)) = d

-- Carefully pick Haskell's built-in type classes which this type should be an instance of.  You should define — and when meaningful overload — simple arithmetic and comparison operations on these fractions (at least: *, /, +, -, neg (negation), <=, >=, <, >, ==).  Also define functions numerator and denominator to return the numerator and denominator, and functions whole and fractional to extract the whole and the fractional part of the fraction.  For example, for (23, 22), whole should return 1, and fraction should return 1 / 22.  Function fractional should return a Frac data type.

-- Return whole portion of a Frac instance
whole :: Frac -> Integer
whole x = (numerator x) `div` (denominator x)

-- Return fractional portion of a Frac instance
fractional :: Frac -> Frac
fractional x = Frac(
                (numerator x) `mod` (denominator x),
                (denominator x)
               )

instance Eq Frac where
  (==)         = compareFrac (==)

instance Ord Frac where
  (<)          = compareFrac (<)
  (<=)         = compareFrac (<=)
  (>)          = compareFrac (>)
  (>=)         = compareFrac (>=)

instance Num Frac where
  (+)          = commonOperateFrac (+)
  (-)          = commonOperateFrac (-)
  (*)          = multiplyFracs
  negate x     = Frac(negate (numerator x), (denominator x))
  abs          = absFrac
  signum       = signumFrac
  fromInteger x = Frac (x, 1)

instance Fractional Frac where
  x / y        = multiplyFracs x (recip y)
  recip        = inverseFrac
  fromRational = fromRational


-- Use Bool operators on the Fractional value of 2 Frac instances
compareFrac :: Fractional(a) => (a -> a -> Bool) -> Frac -> Frac -> Bool
compareFrac f x y = f (evalFrac x) (evalFrac y)

-- Apply Num operators that require 2 Frac instances in commonDenominator form
-- see: (+), (-)
commonOperateFrac :: (Integer -> Integer -> Integer) -> Frac -> Frac -> Frac
commonOperateFrac f x y =
  simplify(
    Frac(
      f (numerator x') (numerator y'),
      (denominator x')
    )
  )
  where x' = commonDenominator x y
        y' = commonDenominator y x

-- Multiply two Frac instances
multiplyFracs :: Frac -> Frac -> Frac
multiplyFracs x y =
  simplify(
    Frac(
      ((numerator x) * (numerator y)),
      ((denominator x) * (denominator y))
    )
  )

-- Invert a Frac instance
inverseFrac :: Frac -> Frac
inverseFrac x = Frac ((denominator x) * (signum xn), abs (xn))
  where xn = numerator x

-- Calcualte absolute value of a Frac instance
absFrac :: Frac -> Frac
absFrac x = Frac(abs (numerator x), (denominator x))

-- Calculate representative sign for a Frac instance
-- Satisfies the property: abs x * (signum x) = x
signumFrac :: Frac -> Frac
signumFrac x = Frac(signum (numerator x), 1)

------------------------------------------------------------------------------
-- Utility functions, not directly called in instance declarations, used in --
-- more than one place. ------------------------------------------------------
------------------------------------------------------------------------------

-- Calculate the common denominator value of x when associated with y
-- Example:
--    commonDenominator Frac(1,2) Frac(1,3)
--  returns:
--    Frac(3,6)
commonDenominator :: Frac -> Frac -> Frac
commonDenominator x y = Frac (
                          (numerator x) * (denominator y),
                          (denominator x) * (denominator y)
                        )

-- Evaluate a Frac instance as a Fractional number
evalFrac :: Fractional(a) => Frac -> a
evalFrac x = (fromInteger (numerator x)) / (fromInteger (denominator x))

-- Utility function to simplify fraction representations.
-- Based on inbuilt gcd function
simplify :: Frac -> Frac
simplify x = Frac(
               (numerator x) `div` gcd',
               (denominator x) `div` gcd'
             )
  where gcd' = gcd (numerator x) (denominator x)

