module Euler1

where

-- Problem 1: Find the sum of all multiples of 3 or 5 below 1000.
sumOfMultiples :: Integer -> Integer
sumOfMultiples 0 = 0
sumOfMultiples n | (((rem n 3) == 0) || ((rem n 5) == 0)) = n + sumOfMultiples (n - 1)
                 | otherwise = 0 + sumOfMultiples (n - 1)

main :: IO ()
main = do
    print $ (sumOfMultiples 999)