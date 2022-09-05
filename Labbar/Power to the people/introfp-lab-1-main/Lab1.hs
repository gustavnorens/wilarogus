{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT440, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : William Flodin Eriksson, Gustav Norén, Aron Karlsson
Lab group   : grupp 10
-}


-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.
import MeasureTime
power :: Integer -> Integer -> Integer
power n k 
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


-- Part A ----------------------------------------------------------------------

-- stepsPower k gives the number of multiplications executed by power n k
stepsPower :: Integer -> Integer
stepsPower k 
  | k < 0     = error "power: negative argument"
  | otherwise = k


-- Part B ----------------------------------------------------------------------

power1 :: Integer -> Integer -> Integer
power1 n k 
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
  | k > 0 = product (replicate (fromInteger k) (fromInteger n))


{- Vi bestämde oss för att skapa två olika funktioner. En som använde sig av List comprehension och
en som använde den inbyggda funktionen replicate. -}
power1Alt :: Integer -> Integer -> Integer
power1Alt n k 
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
  | k > 0 = product [n | x <- [1..k]]


-- Part C ----------------------------------------------------------------------

power2 :: Integer ->Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
  | k `mod` 2 == 0 = power2 (n*n) (k `div` 2)  
  | otherwise = n * power2 n (k-1)



-- Part D ----------------------------------------------------------------------

test1 = power 2 3 == power1 2 3
test2 = power 2 0 == power1 2 0

test3 = power 2 3 == power2 2 3
test4 = power 2 0 == power2 2 0
test5 = power 2 4 == power2 2 4

{- 
Test 1-2 är för funktionen power1 från uppgift B, 
test1 kollar om positiv exponent stämmer
test2 ser om exponenten 0 stämmer

test 3-5 är för funktionen power2 från uppgift C
test3 kollar om positiv och udda exponent stämmer
test4 ser om exponenten 0 stämmer 
test5 ser om positiv och jämn exponent stämmer
-}

comparePower1 :: Integer -> Integer -> Bool
comparePower1 x y = power x y == power1 x y

comparePower2 :: Integer -> Integer -> Bool
comparePower2 x y = power x y == power2 x y

testAll :: Bool
testAll =
  test1 && test2 && test3 && test4 && test5

--Parf f
table :: Integer -> Integer -> IO ()
table n k = putStr (unwords [show k, show (power n k), show (power1 n k), show (power2 n k)])