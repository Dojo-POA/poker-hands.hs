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
rank hand =
  if (numberOfSuits hand) == 1 then Flush
  else if (numberOfFaces hand) == 4 then OnePair
  else if (numberOfFaces hand) == 2 then FourOfAKind
  else HighCard
  where 
    numberOfSuits = length . uniqueSuits
    numberOfFaces = length . uniqueFaces
    uniqueSuits = nub . allSuits
    uniqueFaces = nub . allFaces
    allSuits = map snd
    allFaces = map fst