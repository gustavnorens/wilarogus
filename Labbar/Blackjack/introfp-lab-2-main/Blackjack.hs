{-
Grupp 10
William Flodin,
Gustav Norén
Aron Karlsson
-}

module Blackjack where
import Cards
import RunGame

-- | Constructor for the card Ace of Hearts
aCard1 :: Card
aCard1 = Card Ace Hearts

-- | Constructor for the card Ace of Spades
aCard2 :: Card
aCard2 = Card Ace Spades

-- | Contructor for the 7 of diamonds
aCard3 :: Card
aCard3 = Card (Numeric 7) Diamonds

-- | Constructor of a hand containing the three cards mentioned above
aHand :: Hand
aHand = [aCard1, aCard2, aCard3]
-- | Constructor of a hand containing 2 of Hearts and Jack of Spades
hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card  Jack Spades]

-- | Våran implementation av size hand2
{-
size hand2
  = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
  = 1 + size Card Jack Spades : []
  = 1 + 1 + size [0]
  = 1 + 1 + 0
  = 2,
-}

-- | Våran implementation av sizeSteps
sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2
            ]

-- | Funktion som given en rank returnerar en sträng med namnet på Ranken
displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank x = show x

-- | Funktion som given ett kort returnerar namnet på kortet t.ex "Jack of Spades"
displayCard :: Card -> String
displayCard (Card r s) = displayRank r ++ " of " ++ show s

-- | Funktion som given en hand returnerar en sträng med namnen på alla kort i handen
display :: Hand -> String
display [] = ""
display (x:xs) = displayCard x ++ "\n" ++ display xs

-- | Funktion som given en rank returnerar ett värdet på ranken i form av en int
valueRank :: Rank -> Int
valueRank (Numeric x) = x
valueRank r 
  | r == Ace = 11
  | r == King = 10
  | r == Queen = 10
  | r == Jack = 10

-- | Funktion som given ett kort returnerar värdet på kortet i form av en int
valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

-- | Funktion som returnerar mängden ess i en given hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (x:xs) 
  | valueCard x == 11 = 1 + numberOfAces xs
  | otherwise = numberOfAces xs

-- | Returnerar det sammanlagda värdet på den givna handen använder sig av funktionen
-- | valueCheck för att kolla om värdet är större än 21 och sedan kör value igen och
-- | byter all ess som har värde 11 till värde 1 och räknar ut värdet igen
value :: Hand -> Int
value [] = 0
value (x:xs) 
  | valueCheck (x:xs) > 21 = (valueCard x + value xs) - 10 * numberOfAces (x:xs)
  | otherwise = valueCard x + value xs

-- | Kollar det orginella värdet på value där ett ess är värt 11
valueCheck :: Hand -> Int 
valueCheck [] = 0 
valueCheck (x:xs) = valueCard x + valueCheck xs

-- | Funktion som kollar om den givna handen har bustat
gameOver :: Hand -> Bool
gameOver h
  | value h > 21 = True
  | otherwise = False

-- | Funktion som returnerar en vinnare av två givna händer
winner :: Hand -> Hand -> Player
winner g b 
  | gameOver g == True = Bank
  | value b >= value g && value b < 22 = Bank
  | otherwise = Guest