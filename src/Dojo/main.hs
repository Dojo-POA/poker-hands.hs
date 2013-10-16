module Dojo.Main where

data Face = Two 
          | Three 
          | Four 
          | Five 
          | Six 
          | Seven 
          | Eight 
          | Nine 
          | Ten 
          | Jack 
          | Queen 
          | King 
          | Ace 
  deriving (Show, Eq, Ord, Enum)

data Suit = Clubs | Hearts | Diamonds | Spades
  deriving (Show, Eq)

data Rank = Flush
  deriving (Show, Eq)

type Card = (Face, Suit)
type Hand = [Card]

rank hand = Flush
