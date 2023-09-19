module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck

-- This function checks if one list is a derangement of the other list by looking
-- at whether both lists contain the same elements and whether one element is not
-- on the same index in both lists.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] x = False
isDerangement x y = all (\ z -> (elem z x) && (elem z y) && ((elemIndex z x) /= (elemIndex z y))) x

-- This function creates a list of all derangements of a list [0..n-1] by generating
-- the list of all permutations and filtering out all lists that are not derangements.
deran:: Int -> [[Int]]
deran n = filter (\ x -> isDerangement [0..n-1] x) (permutations [0..n-1])

-- This function does the opposite of the function above. It filters out the derangements
-- from the list of permutations of the list [0..n-1]. The result is a list of permutations
-- that are not derangements.
nonderan:: Int -> [[Int]]
nonderan n = filter (\ x -> (isDerangement [0..n-1] x) == False) (permutations [0..n-1])

-- This generator generates an int between 1 and 7, used as a list size parameter
-- for testing. We restrict the size to be between 1 and 7 for the test to not take
-- too long.
sizeGen :: Gen Int
sizeGen = suchThat (arbitrary :: Gen Int) (\ x -> (x > 1) && (x < 7))

-- This generator generates a list of random size of at least 2, containing all
---unique values.
uniqueListGen :: Eq a => Gen a -> Gen [a]
uniqueListGen gen = do
    n <- suchThat getSize (>= 2)
    k <- choose (1, n)
    fmap nub (vectorOf k gen)

-- Since the empty list is a derangement of itself, the result of isDerangement
-- on two empty lists should always be true.
prop_empty = isDerangement ([]::[Int]) ([]::[Int]) == True

-- Two lists containing only one element can never be a derangement of eachother so
-- the result of the isDerangement function of two singleton lists should always be false.
prop_single x y = isDerangement [x] [y] == False

-- The function deran generates all derangements of a list [0..n-1]. So the result of
-- the isDerangement function on the list [0.n-1] and any list from the deran output,
-- should always be true. The point of this property is to test the function on lists
-- that are known to be derangements of eachother. I wonder if the deran function is
-- okay to use for this, since its definition already assumes that the isDerangement
-- function is correct. I do not know how to implement the deran function without using
-- the isDerangement function.
prop_deran n = all (\ x -> isDerangement [0..n-1] x) (deran n)

-- The function nonderan generates all permutations that are not derangements. So the
-- result of the isDerangement function on the list [0..n-1] and any list from the nonderan
-- output, should always be false. The point of this property is to test the function on lists
-- that are known to not be derangements of eachother. For the same reason as mentioned above,
-- I wonder if the nonderan function is okay to use for this, but I do not know how to implement
-- it otherwise.
prop_nonderan n = all (\ x -> (isDerangement [0..n-1] x) == False) (nonderan n)

-- Any even list is a derangement of its reverse. So the result of the isDerangement on
-- a even list and its reverse should always be true.
prop_reverse n = isDerangement [0..n-1] (reverse [0..n-1]) == True

-- A list can never be a derangement of itself. So the result of the isDerangement
-- function on two identical lists, should always be false.
prop_same n = isDerangement [0..n-1] [0..n-1] == False

main :: IO ()
main = do
    quickCheck prop_empty
    quickCheck (forAll (arbitrary :: Gen Int) prop_single)
    quickCheck (forAll sizeGen prop_deran)
    quickCheck (forAll sizeGen prop_nonderan)
    quickCheck (forAll (suchThat getSize (\ x -> (mod x 2 == 0) && (x >= 2))) prop_reverse)
    quickCheck (forAll (suchThat getSize (>= 2)) prop_same)


{-
--------- LIBRARIES ---------



--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------



--------- TESTING APPROACH ---------



-}