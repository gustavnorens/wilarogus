{-
Grupp 10
William Flodin,
Gustav Norén
Aron Karlsson
-}



module Blackjack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Task A1
-- | Constructor of a hand containing 2 of Hearts and Jack of Spades
hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

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

-- Task A2
-- | Constructor for the card 9 of Hearts
aCard1 :: Card
aCard1 = Card (Numeric 9) Hearts

-- | Constructor for the card Queen of Spades
aCard2 :: Card
aCard2 = Card Queen Spades

-- | Constructor of a hand containing the two cards mentioned above
aHand :: Hand
aHand = []

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

-- Task A3
-- | Funktion som given en rank returnerar ett värdet på ranken i form av en int
valueRank :: Rank -> Int
valueRank (Numeric x) = x
valueRank r 
  | r == Ace = 11
  | otherwise = 10

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
-- | value' för att kolla om värdet är större än 21 och sedan kör value igen och
-- | byter all ess som har värde 11 till värde 1 och räknar ut värdet igen

value :: Hand -> Int
value [] = 0
value (x:xs) 
  | value' (x:xs) > 21 = val (x:xs) - 10 * numberOfAces (x:xs)
  | otherwise = val (x:xs)
  where 
    val = value' :: Hand -> Int 
    value' [] = 0 
    value' (x:xs) = valueCard x + value' xs 

--Task A4
-- | Funktion som kollar om den givna handen har bustat
gameOver :: Hand -> Bool
gameOver h
  | value h > 21 = True
  | otherwise = False

-- | Funktion som returnerar en vinnare av två givna händer
winner :: Hand -> Hand -> Player
winner g b 
  | gameOver g = Bank
  | value b >= value g && value b < 22 = Bank
  | otherwise = Guest

-- | Helper funktion som ger all valörer till fullDeck 
giveRanks :: [Rank]
giveRanks = [Ace, King, Queen, Jack] ++ [Numeric x | x <-[10, 9..2]]

-- | Helper funktion som ger all färger till fullDeck
giveSuit :: [Suit]
giveSuit = [
  Hearts,
  Spades,
  Diamonds,
  Clubs]

-- | Funktion som ger ut en full och oblandad kortlek
fullDeck :: Deck
fullDeck = [Card x y | x <- giveRanks , y <- giveSuit]

-- | Funktion som drar ett kort och lägger det i en hand. Sedan skickar 
-- | funktionen tillbaka en tuple av kortleken och den nya handen
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "Deck is empty."
draw (x:xs) y = (xs, (x:y))

-- | Funktion som spelar för banken 
playBank :: Deck -> Hand 
playBank xs = playBank' xs []

-- | Helper funktion till playBank som ser till att playbank inte behöver någon hand input när den körs.
playBank' :: Deck -> Hand -> Hand
playBank' xs ys 
  | value ys > 15 = ys
  | otherwise = playBank' (fst (draw xs ys)) (snd (draw xs ys))

-- | Funktion som given en lista med doubles mellan 0-1 blandar korten i den ordningen 
-- | som double listan bestämmer. Vad vi gör är att vi tar ut ett slumpmässigt kort ur den oblandade
-- | kortleken a och lägger in det som första värdet i den nya kortleken b. Sedan kallar vi funktionen
-- | en gång till fast denna gången med den nya kortleken a som nu har 51 kort. Slutligen returneras en
-- | ny lista med 52 kort i blandad ordning.*
shuffle :: [Double] -> Deck -> Deck
shuffle [] _ = []
shuffle _ [] = []
shuffle (x:xs) ds 
  | round (x * toEnum (length ds)) == 0 = ds!!(0) : 
    [] ++ shuffle xs (removeCard 0 ds)
  | otherwise = ds!!((round (x * toEnum (length ds))) - 1) : 
    [] ++ shuffle xs (removeCard (round (x * toEnum (length ds) - 1)) ds)

-- | Helper funktion till shuffle som ser till att kortet som flyttas från kortlek a till b inte can dras 2 gånger.
removeCard :: Int -> [a] -> [a]
removeCard _ [] = []
removeCard i (x:xs)
   | i == 0 = xs
   | otherwise = x : removeCard (i-1) xs

-- | Funktion som vi fått
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

-- | Funktion som vi fått
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

-- | Funktion som jämför storleken på två kortlekar före och efter man blandat
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand rs) ds = size ds == size (shuffle rs ds)

-- | Givna funktioner
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

-- | Init
main :: IO ()
main = runGame implementation