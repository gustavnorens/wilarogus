{- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Simplify where

import Poly
import Test.QuickCheck

-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp deriving Eq

--------------------------------------------------------------------------------
-- * A1
-- Define a data type 'Expr' which represents three kinds of expression:
-- binary operators (use 'BinOp' as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should *not* use 'String' or 'Char' anywhere, since this is
-- not needed.

data Expr = Num Int | Binary BinOp Expr Expr | Expo Int

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Expo n)                = if n < 0 then False else True
prop_Expr (Binary _  expr1 expr2) = and $ map prop_Expr [expr1, expr2]
prop_Expr _                       = True 

--------------------------------------------------------------------------------
-- * A3
-- Make 'Expr' an instance of 'Show' (along the lines of the example in the 
-- lecture). You can use Haskell notation for powers: x^2. You should show x^1 
-- as just x. 

instance Show Expr where
  show (Binary binOp expr1 expr2)
    | binOp == AddOp = "(" ++ show expr1 ++ " + " ++ show expr2 ++ ")"
    | otherwise      = "(" ++ show expr1 ++ " * " ++ show expr2 ++ ")"
  show (Expo 1)      = "x"
  show (Num n)       = show n
  show (Expo n)      = "x^" ++ show n
  


--------------------------------------------------------------------------------
-- * A4
-- Make 'Expr' and instance of 'Arbitrary'.
-- Now you can check the data type invariant that you defined in A2 using
-- QuickCheck.

-- (Optional)
-- Add a definition of function @shrink :: Expr -> [Expr]@ to 'Arbitrary'
-- which gives hints to QuickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests.

instance Arbitrary Expr
  where arbitrary = frequency [(1, genNum),(1, genExpo),(3, genBinary)]

genNum :: Gen Expr
genNum = do
  i <- choose (-100, 100)
  return (Num i)
genExpo :: Gen Expr 
genExpo = do
  i <- choose (1, 10)
  return (Expo i)
genBinary :: Gen Expr
genBinary = do
  expr1 <- oneof [genNum, genExpo, genBinary]
  expr2 <- oneof [genNum, genExpo, genBinary]
  binOp <- elements [AddOp, MulOp]
  return (Binary binOp expr1 expr2)

--------------------------------------------------------------------------------
-- * A5
-- Define the @eval@ function which takes a value for x and an expression and
-- evaluates it.

eval :: Int -> Expr -> Int
eval i (Binary binOp expr1 expr2) 
  | binOp == AddOp = sum $ map (eval i) [expr1, expr2]
  | otherwise      = product $ map (eval i) [expr1, expr2]
eval i (Num n)     = n
eval i (Expo n)    = i ^ n


--------------------------------------------------------------------------------
-- * A6
-- Define @exprToPoly@ that converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly :: Expr -> Poly
exprToPoly (Num n)                    = fromList [n]
exprToPoly (Expo n)                   = fromList (1 : replicate n 0) 
exprToPoly (Binary binOp expr1 expr2)
  | binOp == AddOp = (exprToPoly expr1) + (exprToPoly expr2)
  | otherwise = (exprToPoly expr1) * (exprToPoly expr2)
-- Define (and check) @prop_exprToPoly@, which checks that evaluating the
-- polynomial you get from @exprToPoly@ gives the same answer as evaluating
-- the expression.

prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly expr i = eval i expr == evalPoly i (exprToPoly expr)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

polyToExpr :: Poly -> Expr
polyToExpr p = listToExpr $ toList p
  where 
    listToExpr :: [Int] -> Expr
    listToExpr [] = Num 0
    listToExpr [x] = Num x
    listToExpr (x:xs)
      | x == 0 = listToExpr xs
      | x == 1 = garbageCollector (Binary AddOp (Expo (length xs)) (listToExpr xs))
      | otherwise = garbageCollector (Binary AddOp (Binary MulOp (Expo (length xs)) (Num x)) (listToExpr xs))
      where
        garbageCollector :: Expr -> Expr
        garbageCollector (Binary AddOp expr (Num 0)) = expr
        garbageCollector expr = expr



    
    
-- Write (and check) a quickCheck property for this function similar to
-- question 6. 

prop_polyToExpr poly i = evalPoly i poly == eval i (polyToExpr poly) 

--------------------------------------------------------------------------------
-- * A8
-- Write a function @simplify@ which simplifies an expression by converting it 
-- to a polynomial and back again.

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property that checks that a simplified expression does not 
-- contain any "junk", where junk is defined to be multiplication by one or 
-- zero, addition of zero, addition or multiplication of numbers, or x to the
-- power of zero. (You may need to fix A7)

prop_noJunk :: Expr -> Bool
prop_noJunk expr =  garbageChecker $ simplify expr
  where 
    garbageChecker (Expo 0)                       = False
    garbageChecker (Binary binOp (Num 0) _)       = False
    garbageChecker (Binary binOp _ (Num 0))       = False
    garbageChecker (Binary MulOp (Num 1) _)       = False
    garbageChecker (Binary MulOp _ (Num 1))       = False
    garbageChecker (Binary binOp (Num _) (Num _)) = False
    garbageChecker (Binary binOp (Expo 0) _)      = False
    garbageChecker (Binary binOp _ (Expo 0))      = False
    garbageChecker (Binary binOp expr1 expr2)     = and $ map prop_noJunk [expr1, expr2]
    garbageChecker expr                           = True 
--------------------------------------------------------------------------------
-- * A10
-- Write two IO functions that read respectively write the difficulty, which is
-- modelled as a natural number. Use the 'diffFile' as file path. Note that the
-- difficulty should never be below zero.

type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = undefined

writeDifficulty :: Difficulty -> IO ()
writeDifficulty = undefined

--------------------------------------------------------------------------------
-- * A11
-- Define the 'play' function that generates a random expression, a random 
-- value for @x@, show the simplified expression and ask the user to solve it. 
-- If the guess is as expected, give a nice feedback message and increase the 
-- difficulty by one. If the guess was wrong, again give feedback and decrease 
-- the difficulty by one. Then play again.

play :: IO ()
play = undefined

--------------------------------------------------------------------------------
