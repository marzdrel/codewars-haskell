module CamelCaseSpec (spec) where
import CamelCase (toCamelCase)
import Test.Hspec

main = hspec spec
spec = do
  describe "toCamelCase" $ do
    it "converts lower case sentence" $ do
      toCamelCase "the_stealth_warrior" `shouldBe` "theStealthWarrior" 
    it "converts upper case sentence" $ do
      toCamelCase "The-Stealth-Warrior" `shouldBe` "TheStealthWarrior"
