module Dojo.MainSpec (spec) where

import Test.Hspec hiding (example)
import Test.QuickCheck
import Control.Exception (evaluate)
import Dojo.Main


spec :: Spec
spec = do
  describe "High card" $ do
    it "draw when highest cards are equal" $ do
      let hand1 = [(Four, 'H')]
      let hand2 = [(Four, 'H')]
      compareHands hand1 hand2 `shouldBe` draw

    it "right highest card wins" $ do
        let h1 = [(Five, 'H')]
        let h2 = [(Six, 'H')]
        compareHands h1 h2 `shouldBe` rightHand

    it "left highest card wins" $ do
        let h1 = [(Three, 'H')]
        let h2 = [(Two, 'H')]
        compareHands h1 h2 `shouldBe` leftHand  

    it "A should win K" $ do
        let h1 = [(Ace, 'H')]
        let h2 = [(King, 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "K should win Q" $ do
        let h1 = [(King, 'H')]
        let h2 = [(Queen, 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "Q should win J" $ do
        let h1 = [(Queen, 'H')]
        let h2 = [(Jack, 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "J should win 10" $ do
        let h1 = [(Jack, 'H')]
        let h2 = [(Ten, 'H')]
        compareHands h1 h2 `shouldBe` leftHand

    it "10 should win 9" $ do
        let h1 = [(Ten, 'H')]
        let h2 = [(Nine, 'H')]
        compareHands h1 h2 `shouldBe` leftHand




