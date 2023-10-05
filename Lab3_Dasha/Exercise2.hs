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

--This function counts the number of survivors.
countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Integer
countSurvivors nMutants listofProperties funct =
--first arg is number of mutants
--second arg is list of properties
--third arg is the function under test


{-

1. Consider the relation between properties and survivors.

-}


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

