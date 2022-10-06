{- |
Module      : Tetris
Description : The Tetris game (main module)
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

module Main where

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game 
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris 
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris 
  { piece  :: (Pos, Shape)  -- ^ The position and shape of the falling piece
  , well   :: Shape         -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]       -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth  = 10
wellHeight = 20

wellSize :: (Int, Int)
wellSize   = (wellWidth, wellHeight)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Pos addition
add :: Pos -> Pos -> Pos
(x1, y1) `add` (x2, y2) = (x1 + x2, y1 + y2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape (snd (piece t)) && shapeSize (well t) == wellSize

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = makeTopRowBlack (Shape (rows (padShape (1, 1) (shiftShape (1, 1) s))))
  where 
    -- | Makes the top row black, rotates it and calls itself recursively so that all edges become black
    makeTopRowBlack :: Shape -> Shape
    makeTopRowBlack (Shape (x:xs)) 
      | x == replicate (length x) (Just Black) = (Shape (x:xs))
      | otherwise = makeTopRowBlack (rotateShape (Shape (makeRowBlack x : xs)))
      -- | Takes a row and makes every square in it black 
      where
        makeRowBlack :: Row -> Row
        makeRowBlack xs = map (\x -> Just Black) xs

-- | Takes a position and Tetris then returns the Tetris with the updated postion of the falling shape
move :: Pos -> Tetris -> Tetris
move p t = Tetris (add p (fst (piece t)), snd (piece t)) (well t) (shapes t)

-- | Uses move to move the falling piece every tick
tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, updatedState t)
  where
    updatedState t
      | collision t = t
      | otherwise = move (0, 1) t
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls (combine w (shiftShape v p))

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = (map doubleToShape rs)
    where 
      doubleToShape :: Double -> Shape
      doubleToShape x = allShapes !! floor (x * (fromIntegral (length (allShapes))))


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris action t
  | action == MoveDown = Just (0, (moveToBottom t))
  | action == MoveRight = Just (0, (movePiece 1 t))
  | action == MoveLeft = Just (0, (movePiece (-1) t))
  | action == Rotate = Just (0, rotate t)
  | collision (move (0, 1) t) = dropNewPiece t
  | otherwise = tick t 
  where
    moveToBottom t 
      | collision (move (0, 1) t) = t
      | otherwise = moveToBottom (move (0, 1) t)

collision :: Tetris -> Bool
collision t =
  collisionRight t ||
  collisionLeft t ||
  collisionDown t ||
  collisionOverlap t

collisionRight :: Tetris -> Bool
collisionRight (Tetris (p, s) w ss)  = fst p + fst (shapeSize s) > wellWidth

collisionLeft :: Tetris -> Bool
collisionLeft (Tetris (p, s) w ss)  = fst p < 0

collisionDown :: Tetris -> Bool
collisionDown (Tetris (p, s) w ss) = snd p + snd (shapeSize s) > wellHeight

collisionOverlap :: Tetris -> Bool
collisionOverlap (Tetris (p, s) w ss)  = overlaps (place (p, s)) w

movePiece :: Int -> Tetris -> Tetris
movePiece i t
  | collision (move (i, 0) t) = t
  | otherwise = move (i, 0) t

rotate :: Tetris -> Tetris
rotate t
  | collision (Tetris (p, rotateShape s) w ss) = t
  | otherwise = Tetris (p, rotateShape s) w ss
  where 
    (Tetris (p, s) w ss) = t

{-
adjust :: Tetris -> Tetris
adjust t 
  | collisionRight t = movePiece (-1) t
  | collisionLeft t = movePiece 1 t
  | collisionDown t = move (0, -1) t
  | otherwise = t
-}

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (p, s) w (x:xs)) = Just ((fst (clearLines (combine w (place (p, s))))), (Tetris (startPosition, x) (snd (clearLines (combine w (place (p, s))))) xs))

clearLines :: Shape -> (Int, Shape)
clearLines (Shape []) = (0, Shape [])
clearLines (Shape (xs)) = (linesRemoved xs, shiftShapeVer (linesRemoved xs) (Shape ((filter keepLine xs))))
  where
    keepLine :: Row -> Bool
    keepLine rs
      | length (filter (\x -> x /= Nothing) rs) == length rs = False
      | otherwise = True
    linesRemoved :: [Row] -> Int
    linesRemoved xs = wellHeight - (length (filter keepLine xs))

