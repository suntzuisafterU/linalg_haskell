{-# LANGUAGE TemplateHaskell #-}

module DotSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Frac
import Vec
import Dot


spec :: Spec
spec = do
  describe "dot product" $ do
    it "[1 2 0 -1] `dot` [0 1 2 3] == -1" $
      (ColumnVec ([1, 2, 0, -1])) `dot` (ColumnVec([0, 1, 2, 3])) `shouldBe` (-1 :: Frac)
    it "Zero vector dot anything is zero vector" $
      (ColumnVec [0,0,0]) `dot` (ColumnVec [1,2,3]) `shouldBe` 0
    it "[1,2,3] `dot` [4,5,6] `shouldBe` 32" $
      (ColumnVec [1,2,3]) `dot` (RowVec [4,5,6]) `shouldBe` 32

  describe "properties of dot product" $ do
    it "u `dot` v == v `dot` u" $
      (u `dot` v) == (v `dot` u) `shouldBe` True
    it "u `dot` u > 0 if u is not Zero_Vec" $
      (u `dot` u) > 0 `shouldBe` True
    it "((k * u) + (p * v)) `dot` w == (k * (u `dot` w)) + (p * (u `dot` w))" $
      -- ((k * u) + (p * v)) `dot` w == (k * (u `dot` w)) + (p * (u `dot` w)) `shouldBe` True
      pendingWith "implement scalar multiplcation and Vec addition"
  describe "magnitude of a vector u = sqrt (u `dot` u)" $ do
     it "magnitude of [3,4]" $
       mag (ColumnVec [3,4]) `shouldBe` 5
     it "magnitude of [2,1,4,2] == 5" $
       mag (ColumnVec [2,1,4,2]) `shouldBe` 5
        where u = (ColumnVec([2,1,-1]))
              v = (ColumnVec([1,3,5]))
              w = (ColumnVec([4,3,2]))
              z = (ColumnVec([0,0,0]))
              k = 2
              p = -3

