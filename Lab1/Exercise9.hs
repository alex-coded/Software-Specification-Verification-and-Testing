-- This program is intended to implement and test two equations.
-- Time spend: 2 hours

module Exercise9

where

-- Imports Exercise 9.
import Data.List
import Test.QuickCheck
import System.Random

-- This function returns True if its arguments are permutations of each other
-- and False otherwise.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem y (permutations x)

-- posGen :: Gen [Integer]
-- posGen =  suchThat (arbitrary :: Gen [Integer]) (\x -> allUnique x)

listGen :: Gen [Integer]
listGen = do
    x <- choose (1, 3)
    y <- choose (1, 3)
    z <- choose (1, 3)
    return (nub [x, y, z])

-- As long as there is no duplicates in the list, the input lists are permutations
-- of each other if they satisfy the following properties:
-- 1. They have the same amount of elements.
-- 2. All elements in the first list are in the second list and vice versa.
-- If there is duplicates in the list, the second property will not be relevant since
-- for example two lists: [1, 2, 2] and [1, 1, 2]
prop_eq1 :: [Integer] -> [Integer] -> Property
prop_eq1 x y = isPermutation x y ==> (length x == length y)

prop_eq2 :: [Integer] -> [Integer] -> Property
prop_eq2 x y = isPermutation x y ==> (all (\item -> elem item x) y && all (\item -> elem item y) x)

prop_eq3 :: [Integer] -> Bool
prop_eq3 x = isPermutation x x == True

main :: IO ()
main = do
    quickCheck . verbose $
        forAll listGen $ \x ->
        forAll listGen $ \y ->
            prop_eq1 x y

    quickCheck . verbose $
        forAll listGen $ \x ->
        forAll listGen $ \y ->
            prop_eq2 x y

    quickCheck $ forAll posGen $ prop_eq3
