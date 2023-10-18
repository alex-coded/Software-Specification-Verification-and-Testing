-- Study: MSc Software Engineering.
-- This program is intended to find the sum of the first 50 reversible prime squares https://projecteuler.net/problem=808
-- Time spent: 5 hours


module Euler808 where

import Test.QuickCheck

isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | even n || n `mod` 3 == 0 = False
  | otherwise = go 5
  where
    go i
      | i * i > n = True
      | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
      | otherwise = go (i + 6)

revNum n = read (reverse (show n))

checkIfRevPrimeSquare n =
  not (isPalindrome n) &&
  isPrime (isqrt n) &&
  isPrime (isqrt (revNum n))
  where
    isqrt = floor . sqrt . fromIntegral
    isPalindrome n' = show n' == reverse (show n')


revPrimeSquares = take 50 [x^2 | x <- [1..], checkIfRevPrimeSquare (x^2)]

main :: IO ()
main = do
  let sumSquares = sum revPrimeSquares
  putStrLn $ "sum of first 50 reversible prime squares = " ++ show sumSquares


{-

The main logic is in the checkIfRevPrimeSquare function, that checks if n is a reversible prime square,
as specified in the condition.
A reversible prime square should:
1) Not be a palindrome
2) Its square root is prime
3) The square root of its reverlse is also prime. In this case we use isPrime and revNum funcitons,
which are optimized and get a result much faster than the normal functions from the Data Prime library

The isPrime uses the 6k +/- 1 rule, which is a basic optimization, reasonably efficient, 
for checking if a number is prime or not. 

The reversiblePrime generates first 50 rversible prime squares using list comprehension.

-}