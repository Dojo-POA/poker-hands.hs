module Dojo.Main where
import Data.Char(digitToInt)
import Data.List(sort, sortBy)

data Face =   Two 
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

type Card = (Face, Suit)
type Hand = [Card]

leftHand = GT
rightHand = LT
draw = EQ

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 =
	compare (faces hand1) (faces hand2)
	where faces = sort . map fst

