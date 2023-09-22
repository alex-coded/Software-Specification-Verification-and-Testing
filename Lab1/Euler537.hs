-- Study: MSc Software Engineering.
-- This program is intended to solve the first Euler problem described on the site:
-- https://projecteuler.net/archives
-- The problem is: Find the sum of all multiples (natural numbers) of 3 or 5 below 1000.
-- Time spent: 1 hours

module Euler537

where

import Lecture1
-- Problem 1: Find the sum of all multiples of 3 or 5 below 1000.

piFunc :: Integer -> Int
piFunc n = length $ (filter (<= n) (primes))

main :: IO ()
main = do
    print (piFunc 100)
