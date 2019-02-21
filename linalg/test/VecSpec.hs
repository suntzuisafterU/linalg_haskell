module VecSpec where

import           Test.Hspec
import           Vec

main :: IO ()
main = hspec $ do
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

  describe "dot product" $ do
    it "Zero vector dot anything is zero vectore" $
      (ColumnVec [0,0,0]) `dot` (ColumnVec [1,2,3]) `shouldBe` 0
    it "[1,2,3] `dot` [4,5,6] `shouldBe` 32" $
      (ColumnVec [1,2,3]) `dot` (RowVec [4,5,6]) `shouldBe` 32
