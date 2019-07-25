module SumdivsqSpec (spec) where

import Sumdivsq (listSquared)
import Test.Hspec

spec :: Spec
spec =
  describe "listSquared" $ do
    it "returns list squared results" $ do
      listSquared 1 250 `shouldBe` [(1, 1), (42, 2500), (246, 84100)]
      listSquared 42 250 `shouldBe` [(42, 2500), (246, 84100)]
      listSquared 250 500 `shouldBe` [(287, 84100)]
