-- Study: MSc Software Engineering.
-- This program is intended to generate a list of counterexamples to the following
-- statement: "If p1,...,pn is a list of consecutive primes starting from 2, then
-- (p1×...×pn)+1 is also prime."
-- Time spent: 2 hours

module Exercise8

where

-- This function was given in Lab0 and returns True when the input is a prime number
-- and False otherwise.
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

-- This function was given in Lab0 and generates a list of prime numbers starting
-- with 2.
primes :: [Integer]
primes = 2 : filter prime [3..]

-- This function generates a list of tuples, containing a list of consecutive primes
-- and the result of multiplying all the elements in that list and adding 1. Such a
-- tuple only makes it into the list if the result is not prime.
counterexamples :: [([Integer], Integer)]
counterexamples = findCounters 2

-- This function takes an integer n and generates a consecutive list of primes starting
-- from 2 of length n. If the result of multiplying all primes in the list and adding 1
-- is not prime, a tuple containing the list of n primes, and the result of the
-- calculation is added to the resulting list. This process is repeated for every integer
-- starting from the initial input n.
findCounters :: Int -> [([Integer], Integer)]
findCounters n | prime result = findCounters (n+1)
               | otherwise = solution ++ findCounters (n+1)
               where lst = take n primes
                     result = (product lst) + 1
                     solution = [(lst, result)]

-- Execute the counterexamples function for manual testing.
main :: [([Integer], Integer)]
main = do
    counterexamples


{-
--------- LIBRARIES ---------

No additional libraries are used in this Haskell program.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

The purpose of this code was to disprove the following statement:
"If p1,...,pn is a list of consecutive primes starting from 2, then (p1×...×pn)+1
is also prime."
This is done by generating a list of counterexamples to this statement. This was done
by incrementally taking a list of consecutive primes and checking if the outcome of the
calculation is indeed prime. So first, check for [2, 3], then [2, 3, 5], then [2, 3, 5, 7] etc.
When the outcome is NOT prime, it is a counterexample for the statement and it is added to
the resulting list of counterexamples.

--------- TESTING APPROACH ---------

No testing is required for this exercise, but to check the implementation of the counterexamples
function, I manually checked the first 10 counterexamples, which were indeed counterexamples.
These are the first 10 counterexamples that I checked:

([2,3,5,7,11,13],30031)
([2,3,5,7,11,13,17],510511)
([2,3,5,7,11,13,17,19],9699691)
([2,3,5,7,11,13,17,19,23],223092871)
([2,3,5,7,11,13,17,19,23,29],6469693231)
([2,3,5,7,11,13,17,19,23,29,31,37],7420738134811)
([2,3,5,7,11,13,17,19,23,29,31,37,41],304250263527211)
([2,3,5,7,11,13,17,19,23,29,31,37,41,43],13082761331670031)
([2,3,5,7,11,13,17,19,23,29,31,37,41,43,47],614889782588491411)
([2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53],32589158477190044731)

If I had to write quickCkeck tests for this exercise, I would have taken the first 20 elements
of the list to test on, and I would have tested the following properties:

- Whether every 2nd element in the tuples is not prime
- Whether the list in every tuple contains consecutive tuples starting with 2
- Whether the 2nd element is actually the result of the calculation described above

-}
