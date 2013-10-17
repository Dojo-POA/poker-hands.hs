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

  describe "Four of a Kind" $ do
    it "is a four of a kind when four cards have the same face" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Two, Diamonds), 
            (Two, Spades), 
            (Ten, Hearts)] `shouldBe` FourOfAKind
