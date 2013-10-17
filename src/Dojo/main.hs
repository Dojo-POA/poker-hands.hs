module Dojo.Main where
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
          | TwoPairs
          | ThreeOfAKind
          | Straight
          | Flush
          | FullHouse
          | FourOfAKind
          | StraightFlush 
          | RoyalStraightFlush
  deriving (Show, Eq, Ord)

type Card = (Face, Suit)
type Hand = [Card]

rankFor :: [Int] -> [Int] -> Bool -> Rank
rankFor [5] _ True = StraightFlush
rankFor [5] _ _ = Flush
rankFor _ [2, 3] _ = FullHouse
rankFor _ [1, 4] _ = FourOfAKind
rankFor _ [1, 1, 1, 1, 1] True = Straight
rankFor _ [1, 1, 1, 2] _ = OnePair
rankFor _ [1, 2, 2] _ = TwoPairs
rankFor _ [1, 1, 3] _ = ThreeOfAKind
rankFor _ _ _ = HighCard

sortedFaces = sort . map fst
faceDistance faces = zipWith distance faces (tail faces)
distance face1 face2 = (fromEnum face2) - (fromEnum face1)
sequencialSorted [Two, Three, Four, Five, Ace] = True 
sequencialSorted sorted = all (== 1) $ faceDistance sorted
sequencial hand = sequencialSorted (sortedFaces hand)

rank :: Hand -> Rank
rank hand = rankFor (ranker snd hand) (ranker fst hand) (sequencial hand)
  where
    ranker :: Eq a => (Card -> a) -> Hand -> [Int]
    ranker op = sort . map length . group . map op
