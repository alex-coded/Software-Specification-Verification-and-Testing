module Exercise8 where

import Data.List
import System.Random
import Test.QuickCheck

-- This function was given in Lab0 and returns True when the input is a prime number
-- and False otherwise.
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

-- This function was given in Lab0 and generates a list of prime numbers starting
-- with 2.
primes :: [Integer]
primes = 2 : filter prime [3..]

-- This function generages a list of tuples, containing a list of consecutive primes
-- and the result of multiplying that list and adding 1. Such a tuple only makes it
-- into the list if the result is not prime.
counterexamples :: [([Integer], Integer)]
counterexamples = findCounters 2

-- This function takes an integer n and generates a consecutive list of primes starting
-- from 2 of length n. If the result of multiplying all primes in the list and adding 1
-- is not prime, a tuple containing the list of n primes, and the result of the
-- calculation is added to the resulting list. This process is repeated for every integer
-- starting from n.
findCounters :: Int -> [([Integer], Integer)]
findCounters n | prime result = findCounters (n+1)
               | otherwise = solution ++ findCounters (n+1)
               where lst = take n primes
                     result = (product lst) + 1
                     solution = [(lst, result)]

main :: IO ()
main = do
    counterexamples


{-
--------- LIBRARIES ---------



--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------



--------- TESTING APPROACH ---------



-}
