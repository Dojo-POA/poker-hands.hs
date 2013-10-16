module Dojo.Main where
import Data.Set (Set)
import qualified Data.Set as Set

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

extractSuit :: Card -> Suit
extractSuit (_, suit) = suit

extractSuits :: Hand -> [Suit]
extractSuits hand = map extractSuit hand

rank :: Hand -> Rank

rank hand = if Set.size(Set.fromList(extractSuits(hand))) == 1 then Flush else HighCard
