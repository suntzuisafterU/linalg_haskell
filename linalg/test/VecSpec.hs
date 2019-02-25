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
    it "column vector scalar multpilcation" $
      u `multr` 2 `shouldBe` v
    it "row vector scalar multiplication" $
      w `multr` 3 `shouldBe` y
  describe "vector addition" $ do
    it "column vector addition" $
      u `add` v `shouldBe` (ColumnVec [3,6,9])
    it "row vector additon" $
      w `add` y `shouldBe` (RowVec [12,16,20])
        where u = (ColumnVec [1,2,3])
              v = (ColumnVec [2,4,6])
              w = (RowVec [3,4,5])
              y = (RowVec [9,12,15])




