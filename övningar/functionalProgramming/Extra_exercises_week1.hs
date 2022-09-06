{- |
Module      : Week1
Description : Extra exercises lecture week 1
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Week1 where

import Test.QuickCheck

-------------------------------------------------------------------------------
-- The definitions from the live code session of Lecture 1B:

data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Eq)

data Colour = Black | Red deriving (Eq, Show)

colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _      = Red

data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False
rankBeats Ace _ = True
rankBeats _ King = False
rankBeats King _ = True
rankBeats _ Queen = False
rankBeats Queen _ = True
rankBeats _ Jack = False
rankBeats Jack _ = True
rankBeats (Numeric x) (Numeric y) = x > y

prop_rank :: Rank -> Rank -> Bool
prop_rank r1 r2 = rankBeats r1 r2 || rankBeats r2 r1 || r1 == r2

data Card = Card 
  { rank :: Rank
  , suit :: Suit 
  } deriving (Eq, Show)

aceOfSpades :: Card
aceOfSpades = Card Ace Spades

-------------------------------------------------------------------------------
-- Extra exercises based on lecture 1B

-- Ex1: define a Person data type using record syntax. The Person should store
-- a list of first names, the last name, birth date (as an Int) and the social
-- security number (sv: personnummer). Such a data typ can be modelled as 
-- follows using a Haskell data type:
--
-- data Person = Person [String] String Int Int Int Int
--
-- Your task is to define an equivalent data type using record syntax, such as
-- we used to define the Card data type above.


-- Ex2: implement a function @cardBeats card1 card1@ that checks if card1 
-- beats card2. This is the case when the two cards have the same suit and
-- card1 has a higher rank as card1. You should implement the function once
-- using the projection fuctions @rank@ and @suit@i, and once using pattern
-- matching. 

cardBeats :: Card -> Card -> Bool
cardBeats card1 card2 = undefined

-- Ex3: define a type synonym Hand that is synom for a list of Card. Google for
-- 'Haskell type synonym' if needed.


-- Ex4: implement a function that selects all the cards from the hand of the 
-- given suit. Use a list comprehension. 

select :: Suit -> Hand -> [Card]
select = undefined

-- Ex5: implement a function that returns the cards from a hand that beat the
-- given card.

betterCards :: Card -> Hand -> [Card]
betterCards = undefined





-------------------------------------------------------
-- The quickCheck "magic" we need to get it to generate arbitrary
-- elements of our new datatypes:
 
instance Arbitrary Suit where
  arbitrary = elements [Spades, Hearts, Diamonds, Clubs]

instance Arbitrary Rank where
  arbitrary = elements $ [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

instance Arbitrary Card where
  arbitrary =
    do r <- arbitrary
       s <- arbitrary
       return (Card r s)
