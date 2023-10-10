-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent:

module Exercise3 where

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


testMutants :: ([Integer] -> Gen [Integer]) -> Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [(Int, [Gen [Bool]])]
-- testMutants mutator m p t = zip ([1..(length p)]) (take m (repeat (mutate' mutator p t 5)))
testMutants mutator m p t = zip ([1..(length p)]) (mapM (\prob-> take m (repeat (mutate' mutator prob t 5))) [p])
                    -- return mapM (\mt-> mutate' mutator p t mt) [1..m]
                    -- return (zip prop (transpose result))

genToBool :: [Gen [Bool]] -> [Bool]
genToBool input = [y | x <- (sequence input), y <- (concat x)]
-- genToBool input = concat input

-- This function returns True is both given arguments evaluates to True and False otherwise.
andGate :: Bool -> Bool -> Bool
andGate b1 b2 | (b1 == True) && (b2 == True) = True
              | otherwise = False

-- repeatM mutator p t = mutate' mutator p t 5 : repeatM
testSubset :: [[Bool]] -> [Bool]
testSubset (b1:[]) = b1
testSubset (b1:b2:t) = testSubset ((zipWith andGate b1 b2):t)

testSubsets :: ([Integer] -> Gen [Integer]) -> Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [[Int], [Bool]]
testSubsets mutator m p t = do
                    subsets <- tail (subsequences [1..(length p)])
                    truthTable <- testMutants mutator m p t
                    subsetsTable <- [lts | (prop, lts) <- truthTable]
                    

                    
-- sortEquivalent :: [[Int], [Bool]] -> [([[Int]],[Bool])]
-- mapM
-- sequence
--fmap / <$>

-- properties  x mutants: test m mutants against each property p in n choises of test arguments.
-- Results of above would be m x p boolean values--stored in [Int, [Bool]] --> Int is the property number and [Bool] the test outcome
-- Mutate all combination of a listwith p on all mutants m.
-- Result of above will be 2^p x m boolean values -- [[Int], [Bool]] where [Int] is a property subset
-- mutate' function returns based of a subset of properties (for example[1,2,3]) list with three list of bools ([[Bool], [Bool], [Bool]]). How should we merge thse three lists to get the coverage of the subset?
-- Put property subsets in the same class if they kill the same amount mutants
-- Sort equivalent classes by numbers of surviving mutants
-- Result of above -- [([[Int]],[Bool])], where each [[Int]] is an equivalence class of property subsets.
-- Two property sets are apparently equivalent if the property sets kill the same mutants.
-- A set of properties apparently implies another set if whenever a mutant survives testing against the first set it also survives testing against the second.
main :: IO ()
main = do
    print "haai"
