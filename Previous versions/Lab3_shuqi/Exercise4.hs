module Exercise4 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Exercise2 (countSurvivors)

-- This function counts the number of killed mutants.
countKilled :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countKilled n prop fun mutator = do
            survivors <- countSurvivors n prop fun mutator
            return (n - survivors)

-- This function calculates the percentage of killed mutants.
strengthPerc :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
strengthPerc n prop fun mutator | n == 0 = return (0)
                                | otherwise = do
                                    killed <- countKilled n prop fun mutator
                                    return ((div killed n) * 100)

main :: IO Int
main = do
    strengthPerc 100 multiplicationTableProps multiplicationTable addElements
