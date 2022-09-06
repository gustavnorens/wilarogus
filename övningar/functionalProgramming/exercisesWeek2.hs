import Test.QuickCheck

-- maxi x y returns the maximum of x and y

maxi :: Integer -> Integer -> Integer
maxi x y
    | x > y     = x
    | otherwise = y


-- sumsq n returns 1*1 + 2*2 + ... + n*n

sumsq :: Integer -> Integer
sumsq n
    | n < 0 = sumsq (-n)
    | n == 0 = 0
    | otherwise = n*n + sumsq (n-1)


-- Towers of hanoi

hanoi :: Integer -> Integer
hanoi n
    | n < 1 = error "error: ringar < 1"
    | n == 1 = 1
    | n > 1 = 1 + 2 * hanoi (n-1)

hanoiAlt :: Integer -> Integer
hanoiAlt n
    | n < 1 = error "error: ringar < 1"
    | otherwise = 2^n - 1


-- Towers of hanoi 4 poles

hanoi4 :: Integer -> Integer
hanoi4 n
    | n == 1 = 1
    | otherwise = 2 * hanoi4 (n - round(sqrt(fromIntegral n * 2 + 1)) + 1) + hanoi (n - (n - round(sqrt(fromIntegral n * 2 + 1)) + 1))