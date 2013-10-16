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

  describe "Four of a Kind" $ do
    it "is a four of a kind when four cards have the same face" $ do
      rank [(Two, Clubs), 
            (Two, Hearts), 
            (Two, Diamonds), 
            (Two, Spades), 
            (Ten, Hearts)] `shouldBe` FourOfAKind
