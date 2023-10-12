-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to randomly generate different kinds of IOLTSs, including valid and invalid
-- IOLTSs. These generators are used to test the validateLTS function from Exercise1 using quickCheck.
-- Time spent: 

module Exercise2 where

--Imports
import Data.List
import Test.QuickCheck
import Mutation
import FitSpec
import Debug.Trace
import MultiplicationTable

-- This function counts the number of surviving mutants by using vectorOf to repeat the mutate function n times.
countSurvivors :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countSurvivors n prop fun mutator = do
            results <- generate (vectorOf n (mutate' mutator prop fun 3))
            passed <- return (filter (\ x -> (all (\y -> y == True) x) && (x /= [])) results)
            return (length passed)

-- This function counts the number of surviving mutants by using recursion to repeat the mutate function n times.
countSurvivors' :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countSurvivors' n prop fun mutator = do
            results <- generate (mutate' mutator prop fun 3)
            passed <- if results /= [] then return (all (\ x -> x == True) results) else return (False)
            if n /= 0
            then do
                res <- countSurvivors (n - 1) prop fun mutator
                if passed == True
                then return (1+res)
                else return res
            else
                if passed == True
                then return 1
                else return 0

-- Shuffles list
shuffleList :: [Integer] -> Gen [Integer]
shuffleList xs = shuffle xs

-- Takes a random sublist of the output list.
subList :: [Integer] -> Gen [Integer]
subList xs = suchThat (sublistOf xs) (/= [])

-- Increments elements in list by 1.
incrementList :: [Integer] -> Gen [Integer]
incrementList xs = return (map (\x -> x + 1) xs)

-- Decrements elements in list by 1, if element is not 0.
decrementList :: [Integer] -> Gen [Integer]
decrementList xs = return (map (\x -> if x /= 0 then (x - 1) else x) xs)

-- Returns a list of a repeating random element, of a random length.
sameElements :: [Integer] -> Gen [Integer]
sameElements xs = do
             n <- choose(1, 20)
             m <- chooseInteger(1, 10)
             vectorOf n (elements [m])

customMutators = [shuffleList, subList, incrementList, decrementList, sameElements]
{-

1. Consider the relation between properties and survivors


Haskell has a helper module Debug.Trace, this can be used to output monadic values to the terminal easily.
For example:
Assume you have a variable result that stores the mutation function's output.
Use the following code to output to the terminal, then return the value in a do block.

result = --mutation function output
 trace (show result) $ return ... --return the value in a do block

-- Applies a mutator to a property and function under test, then returns whether the mutant is killed (False), whether it lives (True), or that the mutant did not change the output (empty list)
--mutate' :: Eq a => (a -> Gen a) -> [a -> Integer -> Bool] -> (Integer -> a) -> Integer -> Gen [Bool]

--number of mutants = 400
--list of properties = properties multiplicationTable
--function  = multiplicationTable
--expected output 0

--To apply mutate 
--mutator = mutators

-}