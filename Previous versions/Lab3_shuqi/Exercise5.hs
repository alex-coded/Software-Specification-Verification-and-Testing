-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent: 12 hours

module Exercise5 where

import Data.List
import Mutation
import Exercise4
import MultiplicationTable
import Test.QuickCheck

-- Where the first argument is the number of mutants (4000 in the FitSpec example), the second
-- argument is the list of properties, and the third argument is the function under test (the
-- multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- Document the effect of which mutations are used and which properties are used on the number of survivors


-- This function tests m mutants against all properties p in m times.
-- Results of this test would be m x p boolean values stored in [Int, [Bool]]
-- --> Int is the property number and [Gen [Bool]] the m times test outcome of the corresponding property.
testMutants :: ([Integer] -> Gen [Integer]) -> Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [(Int, [Gen [Bool]])]
testMutants mutator m p t = zip ([1..(length p)]) (mapM (\prob-> take m (repeat (mutate' mutator prob t 5))) [p])


-- genToBool :: ([Integer] -> Gen [Integer]) -> Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) ->  [Bool]
-- genToBool mutator m p t = do
--                     result <- mapM (== True) (mutate' mutator p t 5)
--                     return result

-- genToBool :: [Gen [Bool]] -> [Bool]
-- genToBool input = generate (sequence input)
-- genToBool input = foldr (concat) input

-- This function returns True is both given arguments evaluates to True and False otherwise.
andGate :: Bool -> Bool -> Bool
andGate b1 b2 | (b1 == True) && (b2 == True) = True
              | otherwise = False

-- This function zips list of boolean lists with a self defined and gate.
-- THis function is intended to conbine the result of multiple properties into a single result.
testSubset :: [[Bool]] -> [Bool]
testSubset (b1:[]) = b1
testSubset (b1:b2:t) = testSubset ((zipWith andGate b1 b2):t)

-- This function is intended to mutate all combination of a list with p on all mutants m and store the result
-- which will be 2^p x m boolean values in [[Int], [Bool]]. [Int] is a property subset and [Bool] the result of the combination of these properties.
testSubsets :: ([Integer] -> Gen [Integer]) -> Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [([Int], [Bool])]
testSubsets mutator m p t = do
                    subsets <- tail (subsequences [1..(length p)])
                    truthTable <- testMutants mutator m p t
                    subsetsTable <- [lts | elem <- truthTable, lts <- elem !! 1]
                    return [([1, 2], [True])]

-- Approach:
-- Step 1: properties  x mutants: test m mutants against each property p in n choises of test arguments.
-- Results of above would be m x p boolean values--stored in [Int, [Bool]] --> Int is the property number and [Bool] the test outcome
-- Step 2: Mutate all combination of a list with p on all mutants m.
-- Result of above will be 2^p x m boolean values -- [[Int], [Bool]] where [Int] is a property subset
-- mutate' function returns based of a subset of properties (for example[1,2,3]) list with three list of bools ([[Bool], [Bool], [Bool]]) these results will be merged into one.
-- Step 3: Put property subsets in the same class if they kill the same amount mutants
-- Step 4: Sort equivalent classes by numbers of surviving mutants
-- Result of above -- [([[Int]],[Bool])], where each [[Int]] is an equivalence class of property subsets.
-- Step 5: Two property sets are apparently equivalent if the property sets kill the same mutants.
-- Step 6: A set of properties apparently implies another set if whenever a mutant survives testing against the first set it also survives testing against the second.
