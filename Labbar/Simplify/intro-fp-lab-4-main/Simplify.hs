{- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

{-
Grupp 10
William Flodin
Gustav Norén
Aron Karlsson
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


-- Data Expr. With an Integer, Binary and Exponent constructor
data Expr = Num Int | Binary BinOp Expr Expr | Expo Int

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
-- Checks if exponent is negative then calls recusivelsy on all expressions to check
-- if the condidtion holds for all possible ones
prop_Expr :: Expr -> Bool
prop_Expr (Expo n)                = if n < 0 then False else True
prop_Expr (Binary _  expr1 expr2) = and $ map prop_Expr [expr1, expr2]
prop_Expr _                       = True 

--------------------------------------------------------------------------------
-- * A3
-- Make 'Expr' an instance of 'Show' (along the lines of the example in the 
-- lecture). You can use Haskell notation for powers: x^2. You should show x^1 
-- as just x. 

-- Show instance for the Expr datatype 
instance Show Expr where
  show (Binary MulOp (Num 1) expr) = show expr
  show (Binary MulOp expr (Num 1)) = show expr
  show (Binary AddOp (Num 0) expr) = show expr
  show (Binary AddOp expr (Num 0)) = show expr
  show (Binary AddOp expr (Num x)) = if x < 0 then 
    show expr ++ show (Num x) else show expr ++ "+" ++ show (Num x)
  show (Binary binOp expr1 expr2)
    | binOp == AddOp = show expr1 ++ "+" ++ show expr2
    | otherwise      = show expr1 ++ "*" ++ show expr2
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

-- Arbitrary instance for the Expr datatype uses multiple generators
-- and uses the genBinary generator recursively so that we can get larger binary numbers
instance Arbitrary Expr
  where arbitrary = frequency [(1, genNum 100),
                               (1, genExpo 50),
                               (3, genBinary)]

genNum :: Difficulty -> Gen Expr
genNum n = do
  i <- choose (-10 + (-10 * n), 10 + (n * 10))
  return (Num i)
genExpo :: Difficulty -> Gen Expr 
genExpo n = do
  i <- choose (1, 1 + n)
  return (Expo i)
genBinary :: Gen Expr
genBinary = do
  expr1 <- oneof [genNum 10, genExpo 9, genBinary]
  expr2 <- oneof [genNum 10, genExpo 9, genBinary]
  binOp <- elements [AddOp, MulOp]
  return (Binary binOp expr1 expr2)


--------------------------------------------------------------------------------
-- * A5
-- Define the @eval@ function which takes a value for x and an expression and
-- evaluates it.

-- Evaluates an expression with a value for x uses recursion for all cases that 
-- are of type binary and pattern matching for Integers and Exponents
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

-- Function that takes an expression and returns a polynomial.
-- again we use recursion for the Binary type to solve all possible Exprs.
exprToPoly :: Expr -> Poly
exprToPoly (Num n)  = fromList [n]
exprToPoly (Expo n) = fromList (1 : replicate n 0) 
exprToPoly (Binary binOp expr1 expr2)
  | binOp == AddOp  = (exprToPoly expr1) + (exprToPoly expr2)
  | otherwise       = (exprToPoly expr1) * (exprToPoly expr2)

-- Define (and check) @prop_exprToPoly@, which checks that evaluating the
-- polynomial you get from @exprToPoly@ gives the same answer as evaluating
-- the expression.

--Checks if the evaluated Expr it the same as the evaluated Polynomial

prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly expr i = eval i expr == evalPoly i (exprToPoly expr)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

-- Takes a polynomial and returns a simplified Expression.
-- Uses recursion for Binary types and the garbage collector function
-- in order to collect junk such as Addition by zero at the end.
polyToExpr :: Poly -> Expr
polyToExpr p = listToExpr $ toList p
  where 
    listToExpr :: [Int] -> Expr
    listToExpr []  = Num 0
    listToExpr [x] = Num x
    listToExpr (x:xs)
      | x == 0     = listToExpr xs
      | x == 1     = garbageCollector 
        (Binary AddOp (Expo (length xs)) (listToExpr xs))
      | otherwise  = garbageCollector 
        (Binary AddOp 
          (Binary MulOp (Expo (length xs)) (Num x)) (listToExpr xs))
      where
        garbageCollector :: Expr -> Expr
        garbageCollector (Binary AddOp expr (Num 0)) = expr
        garbageCollector expr = expr

-- Write (and check) a quickCheck property for this function similar to
-- question 6. 

-- Just like the other prop we check if the evaluation of our simplified Expr
-- is the same as the evaluation for the original poly
prop_polyToExpr poly i = evalPoly i poly == eval i (polyToExpr poly) 

--------------------------------------------------------------------------------
-- * A8
-- Write a function @simplify@ which simplifies an expression by converting it 
-- to a polynomial and back again.

-- Simplifies the Expr by turning it into a poly and back into an Expr again.
simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property that checks that a simplified expression does not 
-- contain any "junk", where junk is defined to be multiplication by one or 
-- zero, addition of zero, addition or multiplication of numbers, or x to the
-- power of zero. (You may need to fix A7)

-- Checks that a simplified functiion doesnt contain any junk.
-- It can only pass if it fails all pattern matches that are considered "Junk".
-- We also know that on line here is 89 characters long but we decided to keep it.
-- in favor of readability and cleanliness of the code
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

-- Two simple IO functions that read respectively write to the difficulty.txt file
type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = do
  diff <- readFile diffFile
  return (read diff)

writeDifficulty :: Difficulty -> IO ()
writeDifficulty diff = writeFile diffFile (show diff)




--------------------------------------------------------------------------------
-- * A11
-- Define the 'play' function that generates a random expression, a random 
-- value for @x@, show the simplified expression and ask the user to solve it. 
-- If the guess is as expected, give a nice feedback message and increase the 
-- difficulty by one. If the guess was wrong, again give feedback and decrease 
-- the difficulty by one. Then play again.

-- Play function that starts the guessing "game". Uses new generators for
-- better and more concise expressions in the game. Also calls itself recursively
-- such that it starts over when you guess either correct or wrong.

play :: IO ()
play = do
  diff <- readDifficulty
  i <- generate (choose (1 + diff, (-1) - diff))
  expr <- generate (genDifficulties diff)
  putStrLn ("Now solve the following expression with x = " ++ (show i))
  putStrLn ("")
  putStrLn (show $ simplify expr)
  putStrLn ("")
  putStr ("@>") 
  answer <- readLn
  if eval i expr == answer 
    then do
      putStrLn ("Well Done, You are good at this!")
      writeDifficulty (diff + 1)
        else do 
          putStrLn ("No it should have been " ++ (show (eval i expr)))
          writeDifficulty (if diff == 0 then diff else diff - 1)
  putStrLn ("")
  play

--We use these to generate better data in A11
genDifficulties :: Difficulty -> Gen Expr
genDifficulties n = do
  binExpo <- (genBinexpo n)
  diff    <- frequency [(2, genNum n),(3, genDifficulties (n-1))]
  num     <- genNum n 
  return (if n > 0 then (Binary AddOp binExpo diff) 
    else (Binary AddOp binExpo num))

genBinexpo :: Difficulty -> Gen Expr
genBinexpo n = do
  expo <- genExpo n
  num  <- genNum' n
  return (Binary MulOp expo num)

genNum' :: Difficulty -> Gen Expr
genNum' n = do
  i <- choose (1, 1 + n)
  return (Num i) 
