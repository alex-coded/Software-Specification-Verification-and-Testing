module Euler537 where

import Lecture1
-- Problem 1: Find the sum of all multiples of 3 or 5 below 1000.

piFunc :: Integer -> Int
piFunc n = length $ (filter (<= n) (primes))

main :: IO ()
main = do
    print (piFunc 100)
