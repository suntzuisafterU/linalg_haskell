{-# LANGUAGE TemplateHaskell #-}

module VecSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Vec

spec :: Spec
spec = do
  describe "instance of Eq" $ do
    it "ColumnVec should not equal RowVec with same numbers" $
      (ColumnVec [1,2,3]) == (RowVec [1,2,3]) `shouldBe` False
    it "ColumnVecs with same entries in the same order should be equivalent" $
      (ColumnVec [1,2,3]) == (ColumnVec [1,2,3]) `shouldBe` True
    it "ColumnVecs with the same entries in a different order should NOT be equal" $
      (ColumnVec [1,3,2]) == (ColumnVec [1,2,3]) `shouldBe` False
    it "RowVecs with same entries in the same order should be equivalent" $
      (RowVec [1,2,3]) == (RowVec [1,2,3]) `shouldBe` True
    it "RowVecs with the same entries in a different order should NOT be equal" $
      (RowVec [1,3,2]) == (RowVec [1,2,3]) `shouldBe` False

  describe "vector scalar multiplication" $ do
    it "scalar multpilcation" $
      pendingWith "implment scalar multi"




