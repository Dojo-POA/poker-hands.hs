module Dojo.Main where
import Data.Char(digitToInt)

type Card = (Char, Char)
type Hand = [Card]

leftHand = GT
rightHand = LT
draw = EQ

faceValue 'A' = 14
faceValue 'K' = 13
faceValue 'Q' = 12	
faceValue 'J' = 11
faceValue 'T' = 10
faceValue face = digitToInt face

compareHands hand1 hand2 =
	compare (faceValue face1) (faceValue face2)
	where 
		(face1, _) = head hand1
		(face2, _) = head hand2
