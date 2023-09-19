-- This program is intended to implement and test two equations.
-- Time spend: 2 hours

-- Imports Exercise 9.
module Exercise9 where
import Data.List
import Test.QuickCheck

-- This function returns True if its arguments are permutations of each other
-- and False otherwise.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem y (permutations x)

-- As long as there is no duplicates in the list, the input lists are permutations
-- of each other if they satisfy the following properties:
-- 1. They have the same amount of elements.
-- 2. All elements in the first list are in the second list and vice versa.
check_isPermutation :: Eq a => [a] -> [a] -> Bool
check_isPermutation x y | (length x /= length y) = False
                        | otherwise = all (\item -> elem item x) y && all (\item -> elem item y) x

posGen :: Gen Integer
posGen = suchThat (arbitrary :: Gen Integer) (>= 0)

-- listGen :: Gen [Integer]
-- listGen = vectorOf posGen (arbitrary :: Gen Integer)

prop_eq x y = (isPermutation x y) == (check_isPermutation x y)

main :: IO ()
main = do
    quickCheck $ forAll posGen $ prop_eq

-- To prove that these statements are true, we would use induction.
