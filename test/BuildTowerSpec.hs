module BuildTowerSpec (spec) where

import BuildTower (buildTower)
import Test.Hspec

spec = describe "buildTower" $ do
    it "tower with 1 floor"  $ buildTower 1 `shouldBe` [ "*"
                                                       ]
    it "tower with 2 floors" $ buildTower 2 `shouldBe` [ " * "
                                                       , "***"
                                                       ]
    it "tower with 3 floors" $ buildTower 3 `shouldBe` [ "  *  "
                                                       , " *** "
                                                       , "*****"
                                                       ]
