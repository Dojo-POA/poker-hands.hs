module Dojo.MainSpec (spec) where

import Test.Hspec hiding (example)
import Test.QuickCheck
import Control.Exception (evaluate)
import Dojo.Main


spec :: Spec
spec = do
  describe "Flush" $ do
    it "is flush when all suits are the same" $ do
      rank [(Two, Hearts), (Four, Hearts), (Five, Hearts), (Six, Hearts), (Ten, Hearts)] `shouldBe` Flush

