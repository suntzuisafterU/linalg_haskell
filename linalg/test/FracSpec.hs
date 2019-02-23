{-# LANGUAGE TemplateHaskell #-}

module FracSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Frac

-- TODO: Add tests that use <statement> `shouldReturn` Left "something here"
-- TODO: <statement> `shouldSatisfy` (not . null) -- <- any predicate
-- TODO: <statement> `shouldThrow` anyException -- <- or other exception names (ex, anyErrorCall, anyIOException, anyArithException)
-- TODO: define own selector predicate.  (ex, <statement> `shouldThrow` (== ExitFailure 1))


spec :: Spec
spec = do
  describe "numerator and denominator functions" $ do
    it "numerator returns correct Integer" $
      (numerator (Frac (2,3))) `shouldBe` (2 :: Integer)
    it "numerator returns negative Integer when Frac created incorrectly" $
      (numerator (Frac (9,-4))) `shouldBe` (-9 :: Integer)
    it "denominator returns correct Integer" $
      (denominator (Frac (9,-4))) `shouldBe` (4 :: Integer)
  describe "whole and fractional functions" $ do
    it "whole returns whole portion of Frac as integer" $
      (whole (Frac (9,4))) `shouldBe` (2 :: Integer)
    it "whole part of negative Frac is negative" $
      (whole (Frac (-9,4))) `shouldBe` (-2 :: Integer)
    it "fractional part of positive Frac is positive" $
      (fractional (Frac (9,4))) `shouldBe` Frac (1,4)
    it "negative fractionsl" $
      (fractional (Frac (-9,4))) `shouldBe` Frac (-1,4)


  -- describe "Addition" $ do
  --   -- TODO: Figure out how to write good QuickCheck properties
  --   -- it "All the addition" $ property $
  --   --   \x y -> (x + y) 
  -- describe "Floating instance" $ do
  --   it "exp (a + b) = exp a * exp b" $ property $
  --     \a b -> (exp (a + b)) == ((exp a) * (exp b))
  --   it "exp (fromInteger 0) = fromInteger 1" $
  --     exp (fromInteger 0 :: Frac) `shouldBe` (fromInteger 1 :: Frac)

