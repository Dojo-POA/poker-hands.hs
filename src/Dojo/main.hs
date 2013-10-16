module Dojo.Main where
--import Data.Set
import Data.List 

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
  deriving (Show, Eq, Ord)

data Rank = HighCard
          | Flush 
  deriving (Show, Eq, Ord)

type Card = (Face, Suit)
type Hand = [Card]

rank :: Hand -> Rank
rank hand = if (numberOfSuits hand) == 1
 then Flush
 else HighCard
 where 
  numberOfSuits = length . uniqueSuits
  uniqueSuits = nub . allSuits
  allSuits = map snd