data Suit = Spades | Clubs | Hearts | Diamonds
    deriving (Show, Eq)

data Colour = Black | Red deriving (Show, Eq)

colour :: Suit -> Colour
colour s | s == Spades = Black
colour s | s == Clubs  = Black
colour _               = Red

colour' :: Suit -> Colour
colour' s = case s of
    Spades -> Black
    Clubs  -> Black
    _      -> Red

data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace   = False
rankBeats Ace _   = True
rankBeats _ King  = False
rankBeats King _  = True
rankBeats _ Queen = False
rankBeats Queen _ = True
rankBeats _ Jack  = False
rankBeats Jack _  = True
rankBeats (Numeric x) (Numeric y) = x > y

