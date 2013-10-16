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
          | OnePair
          | FourOfAKind 
  deriving (Show, Eq, Ord)

type Card = (Face, Suit)
type Hand = [Card]

rank :: Hand -> Rank
rank hand
  | (numberOfSuits hand) == 1 = Flush
  | (numberOfFaces hand) == 4 = OnePair
  | (numberOfFaces hand) == 2 = FourOfAKind
  | otherwise = HighCard
  where 
    numberOfSuits = length . uniqueSuits
    numberOfFaces = length . uniqueFaces
    uniqueSuits = nub . allSuits
    uniqueFaces = nub . allFaces
    allSuits = map snd
    allFaces = map fst