{-# LANGUAGE TemplateHaskell #-}

module LibSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Lib

spec :: Spec
spec = do
  describe "read" $ do
    it "is the inverse of show" $ property $
      \x -> (read . show) x == (x :: Int)
