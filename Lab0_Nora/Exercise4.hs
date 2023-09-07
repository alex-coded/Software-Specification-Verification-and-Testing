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

reversibleStream :: [Integer]
reversibleStream = filter (\ x -> prime (reversal x)) (takeWhile (<10000) primes)

prop_1 x = reversal (reversal x) == x
prop_3 = all prime reversibleStream
prop_7 = all (< 10000) reversibleStream
