module Dojo.MainSpec (spec) where

import Test.Hspec hiding (example)
import Test.QuickCheck
import Control.Exception (evaluate)
import Dojo.Main




spec :: Spec
spec = do
  describe "High card" $ do
    it "draw when highest cards are equal" $ do
      let hand1 = [('4', 'H')]
      let hand2 = [('4', 'H')]
      compareHands hand1 hand2 `shouldBe` draw

    it "right highest card wins" $ do
        let h1 = [('5', 'H')]
        let h2 = [('6', 'H')]
        compareHands h1 h2 `shouldBe` rightHand

    it "left highest card wins" $ do
        let h1 = [('3', 'H')]
        let h2 = [('2', 'H')]
        compareHands h1 h2 `shouldBe` leftHand  

    it "A should win K" $ do
        let h1 = [('A', 'H')]
        let h2 = [('K', 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "K should win Q" $ do
        let h1 = [('K', 'H')]
        let h2 = [('Q', 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "Q should win J" $ do
        let h1 = [('Q', 'H')]
        let h2 = [('J', 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "J should win 10" $ do
        let h1 = [('J', 'H')]
        let h2 = [('T', 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "10 should win 9" $ do
        let h1 = [('T', 'H')]
        let h2 = [('9', 'H')]
        compareHands h1 h2 `shouldBe` leftHand




