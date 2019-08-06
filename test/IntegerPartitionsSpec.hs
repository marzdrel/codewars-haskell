module IntegerPartitionsSpec where

import Data.List (sort)
import IntegerPartitions (indices)
import Test.Hspec

main = hspec $ do
  describe "Integer partitions" $ do
    it "works for quadratic triangles" $ do
      sort (indices 3 2) `shouldBe` [[0,0,2],
        [0,1,1],[0,2,0],[1,0,1],[1,1,0],[2,0,0]]
