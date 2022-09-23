{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]
    
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (row, col) = Shape (replicate col (replicate row Nothing))
-- ** A2
-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape []) = (0, 0)
shapeSize (Shape (x:xs)) = (length x, length (x:xs))

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (Shape []) = 0
blockCount (Shape (x:xs)) = (length x - rowCount x) + (blockCount (Shape xs)) 
  where 
    rowCount :: Row -> Int
    rowCount [] = 0
    rowCount (x:xs)
      | x == Nothing = (1 + rowCount xs)
      | otherwise = rowCount xs
-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (Shape []) = False
prop_Shape (Shape [x]) = areRowsEqual (Shape [x])
prop_Shape (Shape (x:xs)) = areRowsEqual (Shape (x:xs)) && length x >= 1 && length (x:xs) >= 1


areRowsEqual :: Shape -> Bool
areRowsEqual (Shape []) = False
areRowsEqual (Shape [x]) = True
areRowsEqual (Shape (x:xs)) = length x == length (head xs) && areRowsEqual (Shape xs)
-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements [x | x <- allShapes]

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape xs) = Shape (transpose (reverse xs))

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (a, b) s = shiftShapeVer b (shiftShapeHor a s)

shiftShapeHor :: Int -> Shape -> Shape
shiftShapeHor i (Shape [xs]) = Shape ([replicate i Nothing ++ xs])
shiftShapeHor i (Shape (x:xs)) = Shape ((replicate i Nothing ++ x) : rows (shiftShapeHor i (Shape xs)))

shiftShapeVer :: Int -> Shape -> Shape
shiftShapeVer 0 s = s
shiftShapeVer i s = Shape ((replicate (fst (shapeSize s)) Nothing) : rows (shiftShapeVer (i-1) s))

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (a, b) s = padShapeVer b (padShapeHor a s)

padShapeHor :: Int -> Shape -> Shape
padShapeHor i (Shape [xs]) = Shape ([xs ++ replicate i Nothing])
padShapeHor i (Shape (x:xs)) = Shape ((x ++ replicate i Nothing) : rows (padShapeHor (i) (Shape xs)))

padShapeVer :: Int -> Shape -> Shape
padShapeVer 0 s = s
padShapeVer i s = Shape ( rows (padShapeVer (i-1) s) ++ [(replicate (fst (shapeSize s)) Nothing)])

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (a, b) s = padShape (a - ((fst (shapeSize s))), (b - (snd (shapeSize s)))) s  
-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
