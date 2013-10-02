module Dojo.Main where
import Data.Char(digitToInt)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
	deriving (Show, Eq, Ord, Enum)

type Card = (Face, Char)
type Hand = [Card]

leftHand = GT
rightHand = LT
draw = EQ

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 =
	compare face1 face2
	where 
		(face1, _) = head hand1
		(face2, _) = head hand2
