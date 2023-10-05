module Exercise1 where
import Data.List
import Test.QuickCheck
import Mutation

countKilled :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Integer


strengthPerc :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Double
strengthPerc n props fun | n == 0 = 0.0
                         | otherwise = (countKilled n props fun) / n
