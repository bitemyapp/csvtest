module Main where

import qualified Csvtest as C
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Verify that bassbull outputs the correct data" $ do
    it "equals zero" $ do
      theSum <- C.getAtBatsSum "batting.csv"
      theSum `shouldBe` 4858210
