-- Study: MSc Software Engineering.
-- This program is intended to solve the first Euler problem described on the site:
-- https://projecteuler.net/archives
-- The problem is: Find the sum of all multiples (natural numbers) of 3 or 5 below 1000.
-- Time spent: 1 hours

module Euler1

where

-- This function sums all multiples of 3 or 5 below a given integer.
-- Since all multiples have to be natural numbers, the edge case is
-- when the given integer is 0. Then the result will be as well 0.
-- When the given input is greater than 0, this function will recursively
-- search for multiples of 3 or 5. Only if a number is a multiple, the value
-- of this number will be added to the sum.
sumOfMultiples :: Integer -> Integer
sumOfMultiples 0 = 0
sumOfMultiples n | (((rem n 3) == 0) || ((rem n 5) == 0)) = n + sumOfMultiples (n - 1)
                 | otherwise = sumOfMultiples (n - 1)

-- By calling sumOfMultiples with the value 999, this problem is solved.
-- The answer of this problem is 233168. This answer is evaluated as correct
-- on the site, which also suggests that this function is implemented correctly.
-- Another way to test the correctness sumOfMultiples function is to manually calculate the sum
-- in a small range and compair the calculation with the results from this function.
main :: IO ()
main = do
    print $ (sumOfMultiples 999)
