{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

{-
Grupp 10:
  Gustav Norén,
  William Flodin,
  Aron Karlsson
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
-- Rowcount checks how many empty squares there are in a row
blockCount :: Shape -> Int
blockCount (Shape []) = 0
blockCount (Shape (x:xs)) = (length x - rowCount x) +
  (blockCount (Shape xs))
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
prop_Shape (Shape (x:xs)) = areRowsEqual (Shape (x:xs)) &&
  length x >= 1 && length (x:xs) >= 1

-- A function that checks of all the rows on a given shape 
-- has the same size/length
areRowsEqual :: Shape -> Bool
areRowsEqual (Shape []) = False
areRowsEqual (Shape [x]) = True
areRowsEqual (Shape (x:xs)) = length x == length (head xs) &&
  areRowsEqual (Shape xs)
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

-- A helper function that adds empty squares to the left of the shape
shiftShapeHor :: Int -> Shape -> Shape
shiftShapeHor i (Shape []) = Shape []
shiftShapeHor i (Shape (x:xs)) = Shape ((replicate i Nothing ++ x) :
  rows (shiftShapeHor i (Shape xs)))

-- A helper function that adds empty squares below the shape
shiftShapeVer :: Int -> Shape -> Shape
shiftShapeVer i s = Shape ((replicate i (replicate 
  (fst (shapeSize s)) Nothing)) ++ rows s)

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (a, b) s = padShapeVer b (padShapeHor a s)

-- A helper function that adds empty squares to the right of the shape
padShapeHor :: Int -> Shape -> Shape
padShapeHor i (Shape []) = Shape []
padShapeHor i (Shape (x:xs)) = Shape ((x ++ replicate i Nothing) :
  rows (padShapeHor (i) (Shape xs)))

-- A helper function that adds empty squares below the shape
padShapeVer :: Int -> Shape -> Shape
padShapeVer i s = Shape (rows s ++ 
  (replicate i (replicate (fst (shapeSize s)) Nothing)))
-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (a, b) s = padShape 
  (a - ((fst (shapeSize s))), (b - (snd (shapeSize s)))) s  
-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap. Uses rowsOverlap to check if any squares overlap
-- | and then returns true if any squares in the shape does overlap.
overlaps :: Shape -> Shape -> Bool
overlaps (Shape []) _ = False
overlaps _ (Shape []) = False
overlaps (Shape (x:xs)) (Shape (y:ys))
  | rowsOverlap x y = True
  | otherwise = False || overlaps (Shape xs) (Shape ys)

-- | Checks if two given rows overlap by comparing their squares. Turns the rows into tuples
-- | with their value e.g "Nothing" and position. Then we filter away all tuples that have the value
-- | Nothing and so we are left with only colored squares. Now it compares the positions of the 
-- | remaining colors and if either list contains the same element they must overlap.
rowsOverlap :: Row -> Row -> Bool
rowsOverlap xs ys = sameElement 
  (zipLogic (zip xs [1..(length xs)]))
  (zipLogic (zip ys [1..(length ys)]))
  where
    -- | Checks for every value in xs if it exists in ys
    sameElement :: Eq a => [a] -> [a] -> Bool
    sameElement xs ys = xs /= [x | x <- xs, not (elem x ys)]
    -- | Just to make the rowsOverlap function a little bit more compact
    zipLogic :: [(Square, Int)] -> [Int]
    zipLogic xs = snd (unzip (filter (\x -> (fst x) /= Nothing) xs))

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists. Uses zipRowsWith so that zipWith extends to every
-- | square in a given shape instead of just every row.
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f (Shape []) _ = (Shape [])
zipShapeWith f _ (Shape []) = (Shape [])
zipShapeWith f (Shape (x:xs)) (Shape (y:ys)) = Shape 
  (zipRowsWith f x y : rows (zipShapeWith f (Shape xs) (Shape ys)))
  where 
    -- | calls zipWith on every square in two Rows
    zipRowsWith :: (Square -> Square -> Square) ->  Row -> Row -> Row
    zipRowsWith f xs ys = zipWith f xs ys 

-- | Combine two shapes. The two shapes should not overlap.
-- | The resulting shape will be big enough to fit both shapes. Uses two helper functions pads and
-- | compSquares. Pads just to make combine eaiser to read and compSquares which we use with the 
-- | previously defined zipShapeWith to figure out what the squares of the new shape should be.
-- |  
combine :: Shape -> Shape -> Shape
combine s1 s2 = zipShapeWith compSquares (pads s1) (pads s2)
  where 
    pads = padShapeTo (max (fst (shapeSize s1)) (fst (shapeSize s2)), 
      max (snd (shapeSize s1)) (snd (shapeSize s2)))
    compSquares :: Square -> Square -> Square
    compSquares x y
      | x == Nothing && y /= Nothing = y
      | x /= Nothing && y == Nothing = x
      | x == Nothing && y == Nothing = Nothing
      | otherwise = error "overlapping shapes"