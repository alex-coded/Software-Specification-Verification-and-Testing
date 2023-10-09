-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to randomly generate different kinds of IOLTSs, including valid and invalid
-- IOLTSs. These generators are used to test the validateLTS function from Exercise1 using quickCheck.
-- Time spent: 

module Exercise3 where

--Imports
import Data.List
import Test.QuickCheck
import Mutation
--import FitSpec
import MultiplicationTable
import Debug.Trace

--Implement a function that calculates the minimal property subsets, given a 'function under
--test' and a set of properties.

MPS :: -> (Integer -> [Integer]) -> [([Integer] -> Integer -> Bool)] -> [([Integer] -> Integer -> Bool)]
MPS fut properties = 
    --countSurvivors () = 
{-

If a property kills no mutant, or it kills the same mutants as another property, it can be
considered redundant over other properties.

Take a set of properties

If countSurvivors for this property (number of mutants) = number of mutations (all survived -> killed no mutants)
= the property for which it was tested is redundant -> exclude it from set

If mutate (mutator, property1, function, input) = mutate (mutator, property2, function, input) = property2 is redundant 
-> GEORGIA NOTE -> CANNOT DO BC COMPARING DIFFERENT THINGS 

return the minimal set of properties

-}

--GEORGIA - REWRITE THE MUTATE FUNCTION TO INCLUDE TWO PROPERTIES
--Applies a mutator to TWO PROPERTIES and function under test, then returns TRUE IF THE PROPERTIES KILL THE SAME MUTANT
mutate'' :: Eq a => (a -> Gen a) -> (a -> Integer -> Bool) -> (a -> Integer -> Bool) -> (Integer -> a) -> Integer -> Gen Bool
mutate'' mutator prop1 prop2 fut input = mutation >>= \mutant -> propertyExecutor'' prop1 prop2 mutant input
        where output = fut input
              mutation = mutator output

--Returns TRUE IF THE PROPERTIES KILL THE SAME MUTANT 
propertyExecutor'' :: Eq a => (a -> Integer -> Bool) -> (a -> Integer -> Bool) -> a -> Integer -> Gen Bool
propertyExecutor'' prop1 prop2 mutant x = pure (prop1 mutant x == prop2 mutant x)

main :: IO()
main = do
    putStrLn "Testing yourGenFunction with parameter:"
    results <- generate (mutate'' addElements prop_tenElements prop_firstElementIsInput multiplicationTable 3)
    print(results)

