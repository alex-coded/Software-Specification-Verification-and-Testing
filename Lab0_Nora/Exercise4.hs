import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

-- Here, I take numbers from the primes stream, as long as they are less than 10000.
-- From these numbers, I filter out the numbers of which the reverse is not prime.
-- So the resulting list only contains prime numbers under 10000 of which the reverse
-- is also prime.
reversibleStream :: [Integer]
reversibleStream = filter (\ x -> prime (reversal x)) (takeWhile (<10000) primes)

-- This function determines if all elements in a list are unique, used for property 6.
-- I recursively check for every element if they are also present in the rest of the list.
-- By using the && operator, every element has to be unique for the result to be True.
unique :: [Integer] -> Bool
unique [] = True
unique (x:y) = notElem x y && unique y

-- The reversal function does not work on negative numbers or numbers ending on a 0.
-- That is why I filter out those numbers when testing the reversal function. I feel
-- like ignoring these numbers is OK since none of these numbers are prime.
prop_1 x = if (x >= 0) && (rem x 10 /= 0) then reversal (reversal x) == x else True

-- For the prime membership property, I simply check whether all elements in the
-- list are prime.
prop_3 = all prime reversibleStream

-- For the uniqueness property, I use the unique function defined above to check
-- whether all elements in the list are unique.
prop_6 = unique reversibleStream

-- For the final property, I check if all elements in the list are below 10000.
prop_7 = all (< 10000) reversibleStream