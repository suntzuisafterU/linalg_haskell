{-# LANGUAGE TemplateHaskell #-}

module MatSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Frac
import Vec
import Mat


spec :: Spec
spec = do
  describe "Systems of Eqns" $ do
    it "Nothing yet" $
      pending
