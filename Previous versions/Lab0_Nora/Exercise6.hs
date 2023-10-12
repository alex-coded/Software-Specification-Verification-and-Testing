import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

consecutive101Prime :: Integer
consecutive101Prime | 
    do
    x <- take 101 primes
