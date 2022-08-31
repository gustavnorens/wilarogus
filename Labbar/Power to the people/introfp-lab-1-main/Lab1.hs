{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT440, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}


-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.
power :: Integer -> Integer -> Integer
power n k 
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


-- Part A ----------------------------------------------------------------------

-- stepsPower k gives the number of multiplications executed by power n k
stepsPower :: Integer -> Integer
stepsPower k = k 


-- Part B ----------------------------------------------------------------------

power1 :: Integer -> Integer -> Integer
power1 n k 
  | k < 0 = error "error"
  | k == 0 = 1
  | k > 0 = product (replicate (fromInteger k) (fromInteger n))


{- Vi bestämde oss för att skapa två olika funktioner. En som använde sig av List comprehension och
en som använde den inbyggda funktionen replicate. -}
power1Alt :: Integer -> Integer -> Integer
power1Alt n k 
  | k < 0 = error "error"
  | k == 0 = 1
  | k > 0 = product [n | x <- [1..k]]


-- Part C ----------------------------------------------------------------------

power2 :: Integer ->Integer -> Integer
power2 n k
  | k < 0 = error "error"
  | k == 0 = 1
  | k `mod` 2 == 0 = power2 (n*n) (k `div` 2)  
  | otherwise = n * power2 n (k-1)



-- Part D ----------------------------------------------------------------------
test1 = power 9 9 == power1 9 9
test2 = power1 9 9 == power102 9 9
test3 = power1Alt (-8) 9 == power2 (-8) 9

{- 
<Describe your test cases here>

-}

comparePower1 :: Integer -> Integer -> Bool
comparePower1 x y = power x y == power1 x y

comparePower2 :: Integer -> Integer -> Bool
comparePower2 x y = power x y == power2 x y

testAll :: Integer -> Integer -> Bool
testAll x y = comparePower1 x y && comparePower2 x y
