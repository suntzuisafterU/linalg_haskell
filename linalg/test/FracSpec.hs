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
  describe "Addition" $ do
    -- TODO: Figure out how to write good QuickCheck properties
    -- it "All the addition" $ property $
    --   \x y -> (x + y) 

