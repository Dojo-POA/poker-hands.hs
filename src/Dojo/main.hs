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
          | OnePair
          | Flush
          | FullHouse
          | FourOfAKind 
  deriving (Show, Eq, Ord)

type Card = (Face, Suit)
type Hand = [Card]

rankFor [5] _ = Flush
rankFor _ [2, 3] = FullHouse
rankFor _ [1, 4] = FourOfAKind
rankFor _ [1, 1, 1, 2] = OnePair
rankFor _ _ = HighCard

rank :: Hand -> Rank
rank hand = rankFor  (ranker snd hand) (ranker fst hand)
  where ranker op = sort . map length . group . map op
 