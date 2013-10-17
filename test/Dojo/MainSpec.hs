module Dojo.MainSpec (spec) where

import Test.Hspec hiding (example)
import Test.QuickCheck
import Control.Exception (evaluate)
import Dojo.Main


spec :: Spec
spec = do
  describe "Flush" $ do
    it "is flush when all suits are the same" $ do
      rank [(Two, Hearts), 
            (Four, Hearts), 
            (Five, Hearts), 
            (Six, Hearts), 
            (Ten, Hearts)] `shouldBe` Flush
    it "is NOT flush when one suit is NOT the same" $ do
      rank [(Two, Clubs), 
            (Four, Hearts), 
            (Five, Hearts), 
            (Six, Hearts), 
            (Ten, Hearts)] /= Flush
  describe "One Pair" $ do
    it "is a pair when two and only two cards have the same face" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Five, Hearts), 
            (Six, Hearts), 
            (Ten, Hearts)] `shouldBe` OnePair

  describe "Full House" $ do
    it "is a full house when there is a pair and a three of a kind" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Two, Diamonds), 
            (Ace, Spades), 
            (Ace, Hearts)] `shouldBe` FullHouse

  describe "Two Pairs" $ do
    it "is a two pairs when there is two pairs of a same face" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Three, Diamonds), 
            (Ace, Spades), 
            (Ace, Hearts)] `shouldBe` TwoPairs


  describe "Three of a kind" $ do
    it "is a three of a kind when there is three of a kind of a same face" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Two, Diamonds), 
            (Three, Spades), 
            (Ace, Hearts)] `shouldBe` ThreeOfAKind


  describe "Straight" $ do
    it "is a straight when there is a sequence of faces" $ do
      rank [(Two, Clubs), 
            (Three, Hearts), 
            (Four, Diamonds), 
            (Five, Spades), 
            (Six, Hearts)] `shouldBe` Straight
    it "is not a straight when it is not a sequence of faces" $ do
      rank [(Two, Clubs), 
            (Three, Hearts), 
            (Four, Diamonds), 
            (Five, Spades), 
            (Seven, Hearts)] `shouldBe` HighCard
    it "is a straight when it starts with an Ace" $ do
      rank [(Two, Clubs), 
            (Three, Hearts), 
            (Four, Diamonds), 
            (Five, Spades), 
            (Ace, Hearts)] `shouldBe` Straight

  describe "Four of a Kind" $ do
    it "is a four of a kind when four cards have the same face" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Two, Diamonds), 
            (Two, Spades), 
            (Ten, Hearts)] `shouldBe` FourOfAKind

  describe "Straight Flush" $ do
    it "is a four of a kind when four cards have the same face" $ do
      rank [(Two, Hearts), 
            (Three, Hearts), 
            (Four, Hearts), 
            (Five, Hearts), 
            (Six, Hearts)] `shouldBe` StraightFlush

  describe "Royal Straight Flush" $ do
    it "is a four of a kind when four cards have the same face" $ do
      rank [(Ace, Hearts), 
            (King, Hearts), 
            (Queen, Hearts), 
            (Jack, Hearts), 
            (Ten, Hearts)] `shouldBe` RoyalStraightFlush
