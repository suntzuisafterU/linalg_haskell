module Frac where

data Frac = Frac (Integer, Integer) | FloatingFrac (Float)

instance Show Frac where
  show = showFrac

showFrac :: Frac -> String
showFrac (Frac x)
  -- | denominator x == 0  = show (signum x) times infinity -- not implementing
  | dx == 1  = show (nx)
  | otherwise           = "(" ++ show (nx) ++ "/" ++ show (dx) ++ ")"
    where nx = fst x
          dx = snd x
showFrac (FloatingFrac x) = show x

numerator :: Frac -> Integer
numerator (Frac (n, d)) = signum (n * d) * (abs n)

denominator :: Frac -> Integer
denominator (Frac (_, d)) = abs d

-- Return whole portion of a Frac instance
whole :: Frac -> Integer
whole x = signum (n * d) * (abs n `div` d)
  where n = numerator x
        d = denominator x

-- Return fractional portion of a Frac instance
fractional :: Frac -> Frac
fractional x = signum x * Frac(
                (abs n) `mod` (d),
                (d)
               )
  where n = numerator x
        d = denominator x

instance Eq Frac where
  (==)         = compareFrac (==)

instance Ord Frac where
  (<)          = compareFrac (<)
  (<=)         = compareFrac (<=)
  (>)          = compareFrac (>)
  (>=)         = compareFrac (>=)

instance Real Frac where
  toRational x = (((fromIntegral.numerator) x) / ((fromIntegral.denominator) x))

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
  -- fromRational = error "fromRational not implemented" -- We are not implementing this

-- same as properFraction from RealFrac typeclass
-- properties:
--   takes Real Frac x and returns n+f such that:
--      n is an integral number with the same sign as x and
--      f is a fraction of the same type and sign as x and
--        has abs value less than 1
propFrac :: Frac -> (Integer, Frac)
propFrac x = ((whole x),(fractional x))

-- Why did we give up on implementing Floating?
-- Because this is NOT a Floating type.  It can easily be converted to one though...
toFloating :: Floating a => Frac -> a
toFloating x = ( n / d )
  where n = ( fromIntegral.numerator ) x
        d = ( fromIntegral.denominator ) x

toFloatingFrac :: Frac -> Frac
toFloatingFrac x = (FloatingFrac (toFloating x))

instance Floating Frac where

-- Replace required Floating definitions
-- replaces sqrt,
fsqrt :: Floating a => Frac -> a
fsqrt x = (sqrt.toFractional) x

-- Trig definitions for magnitude function on Vec data type
fsin :: Floating a => Frac -> a
fsin x = (sin.toFractional) x

fcos :: Floating a => Frac -> a
fcos x = (cos.toFractional) x

ftan :: Floating a => Frac -> a
ftan x = (tan.toFractional) x

-- convert to Rational number for computations
toFractional :: Fractional a => Frac -> a
toFractional x = (((fromIntegral.numerator) x) / ((fromIntegral.denominator) x))

toFloat :: Frac -> Float
toFloat x = (((fromIntegral.numerator) x) / ((fromIntegral.denominator) x))

-- Use Bool operators on the Fractional value of 2 Frac instances
compareFrac :: Fractional a => (a -> a -> Bool) -> Frac -> Frac -> Bool
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

