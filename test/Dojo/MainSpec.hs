module Dojo.MainSpec (spec) where

import Test.Hspec hiding (example)
import Test.QuickCheck
import Control.Exception (evaluate)
import Dojo.Main


spec :: Spec
spec = do
  describe "High card" $ do
    it "draw when highest cards are equal" $ do
      let hand1 = [(Four, Hearts)]
      let hand2 = [(Four, Hearts)]
      compareHands hand1 hand2 `shouldBe` draw

    it "right highest card wins" $ do
        let h1 = [(Five, Hearts)]
        let h2 = [(Six, Hearts)]
        compareHands h1 h2 `shouldBe` rightHand

    it "left highest card wins" $ do
        let h1 = [(Three, Hearts)]
        let h2 = [(Two, Hearts)]
        compareHands h1 h2 `shouldBe` leftHand  

    it "A should win K" $ do
        let h1 = [(Ace, Hearts)]
        let h2 = [(King, Hearts)]
        compareHands h1 h2 `shouldBe` leftHand

    it "K should win Q" $ do
        let h1 = [(King, Hearts)]
        let h2 = [(Queen, Hearts)]
        compareHands h1 h2 `shouldBe` leftHand

    it "Q should win J" $ do
        let h1 = [(Queen, Hearts)]
        let h2 = [(Jack, Hearts)]
        compareHands h1 h2 `shouldBe` leftHand

    it "J should win 10" $ do
        let h1 = [(Jack, Hearts)]
        let h2 = [(Ten, Hearts)]
        compareHands h1 h2 `shouldBe` leftHand

  describe "Two hands in diferent orders" $ do
    it "10 should win 9" $ do
        let h1 = [(Ten, Hearts),(Nine, Clubs)]
        let h2 = [(Nine, Hearts), (Eight, Clubs)]
        compareHands h1 h2 `shouldBe` leftHand

    it "10 should win 9 different order" $ do
        let h1 = [(Nine, Clubs), (Ten, Hearts)]
        let h2 = [(Nine, Hearts), (Eight, Clubs)]
        compareHands h1 h2 `shouldBe` leftHand

  describe "Pair beats high card" $ do
    it "two kings wins against one ace when left hand" $ do
        let h1 = [(King, Hearts), (King, Spades)]
        let h2 = [(Ace, Hearts), (Two, Clubs)]
        compareHands h1 h2 `shouldBe` leftHand

    it "two sevens wins against one ace when right hand" $ do
        let h1 = [(Ace, Hearts), (Two, Clubs)]
        let h2 = [(Seven, Hearts), (Seven, Spades)]
        compareHands h1 h2 `shouldBe` rightHand

    it "two twos wins against one ace" $ do
        let h1 = [(Six, Hearts), (Two, Clubs), (Two, Hearts), (Three, Hearts), (Four, Diamonds)]
        let h2 = [(Ace, Diamonds), (Jack, Spades), (King, Spades), (Ten, Spades), (Nine, Spades)]
        compareHands h1 h2 `shouldBe` leftHand

  describe "Three of a kind beats high card" $ do
    it "three fours wins against high card" $ do
        let h1 = [(Ace, Hearts), (King, Spades), (Two, Diamonds)]
        let h2 = [(Four, Hearts), (Four, Clubs), (Four, Spades)]
        compareHands h1 h2 `shouldBe` rightHand
