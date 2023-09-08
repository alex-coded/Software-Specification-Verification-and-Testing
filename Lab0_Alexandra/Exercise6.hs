

import Data.Numbers.Primes (primes, isPrime)

smallestSumOfConsecutivePrimes = findSmallestPrime 101 primes

findSmallestPrime n (curPrime:listRemainingPrimes)
    | isPrime possibleSol = possibleSol
    | otherwise = findSmallestPrime n listRemainingPrimes
    where
        possibleSol = sum (take (fromIntegral n) (curPrime:listRemainingPrimes))


main :: IO ()
main = print smallestSumOfConsecutivePrimes


{-

-----Libraries-----
Used the library primes (Data.Numbers.Primes) for getting the list of primes and
for testing our sum and the other primes for this property. 


-----Code description (purpose, features, arhitecture)-----
Our approach of finding the smallest possible number that is also the sum of n (101) consecurive primes
lies in summing up the first n primes, beginning with current prime. As per se, the main logic is in the
function findSmallestPrime, where we sum up the primes until we get a sum that is also prime. 
Until we find the solution, we search recursively by calling the function for the list of prime numbers (Primes library).
In short, we search in a systematic approach the primes sum, until we get to the desired results
that meets the criteria.


------Testing approach------

We could do the testing approach by checking with a personal function if the criteria is respected (for the adding numbers,
as well as the sum):


myIsPrime n
    | n <= 1    = False
    | n <= 3    = True
    | otherwise = all (\x -> n `mod` x /= 0) [2..upperBound]
    where
        upperBound = floor (sqrt (fromIntegral n))


On another note, we do not need to prove that the number is the smalles we could get,
because as per se, the further we go into the sum of the primes list with our verification, 
the bigger the sum is. By their nature, the prime numbers become less frequent and larger when you 
go further down the list. By this, I can say that the code is a practical solution to the given problem


I also did a mathematical test, adding up the numbers starting with 83, and it yielded the desired output of
    37447. In this way, I mathematically checked the correctness of my program.

-}

