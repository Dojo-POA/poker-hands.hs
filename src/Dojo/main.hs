module Dojo.Main where
import Data.Char(digitToInt)
import Data.List(sort)

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

type Card = (Face, Char)
type Hand = [Card]

leftHand = GT
rightHand = LT
draw = EQ

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 =
	compare (sort hand1) (sort hand2)