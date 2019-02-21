module VecSpec where

import           Test.Hspec
import           Vec

main :: IO ()
main = hspec $ do
  describe "instance of Eq" $ do
    it "ColumnVec should not equal RowVec with same numbers" $
      (ColumnVec [1,2,3]) == (RowVec [1,2,3]) `shouldBe` False
