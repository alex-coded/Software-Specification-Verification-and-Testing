-- Study: MSc Software Engineering.
-- This program is intended to solve the first Euler problem described on the site:
-- https://projecteuler.net/archives
-- The problem is: What is the 10001st prime number?
-- Time spent: 0.5 hours

module Euler7

where

import Lecture1

-- This function retrieves the nth prime number.
nthPrime :: Int -> Integer
nthPrime n = primes !! (n - 1)

-- By calling nthPrime with the value 10001, we retrieve the 10001th prime number.
-- The answer of this problem is 104743. his answer is evaluated as correct
-- on the site, which also suggests that this function is implemented correctly.
-- A way to test this function is to test whether this answer is a prime number.
main :: IO ()
main = do
    print (nthPrime 10001)
