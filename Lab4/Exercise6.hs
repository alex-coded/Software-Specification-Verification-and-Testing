-- Study: MSc Software Engineering.
-- This file is intended to test the functionality of functions "symClos" and "trClos" from Exercise3.hs and Exercise5.hs.
-- Time spent: 120 minutes

module Exercise6 where

import Exercise3(symClos)
import Exercise5(trClos)
import SetOrd
import Test.QuickCheck

type Rel a = [(a,a)]

-- This function checks whether the following property of symmetric closures generated by symClos holds.
-- Symmetricity: for every tuple (x,y) look if there is (y,x).
isSymmetric :: (Eq a, Ord a) => Rel a -> Bool
isSymmetric r = let symmetric = symClos r
                in and [elem (y, x) symmetric | (x, y) <- symmetric]

-- This function checks whether all closures in the original relation is in the symmetric version.
subsetSymmetric :: (Eq a, Ord a) => Rel a -> Bool
subsetSymmetric r = let symmetric = symClos r
                    in and [elem c symmetric | c <- r]

-- This function checks whether the following property of transitive closures generated by trClos holds.
-- Transtivity: for every closure (x,y) where (y,z) is a closure, (x,z) should also be a closure.
isTransitive :: (Eq a, Ord a) => Rel a -> Bool
isTransitive r = let transitive = trClos r
                 in and [elem (x, t) transitive| (x, y) <- transitive, (z, t) <- transitive, y == z]

-- This function checks whether all closures in the original relation is in the transitive version.
subsetTransitive :: (Eq a, Ord a) => Rel a -> Bool
subsetTransitive r = let transitive = trClos r
                    in and [elem c transitive | c <- r]

-- Using quick check to create arbitrary inputs for the four tests.
main :: IO ()
main = do
    quickCheck $ forAll (arbitrary :: Gen [(Int, Int)])$ isSymmetric
    quickCheck $ forAll (arbitrary :: Gen [(Int, Int)])$ subsetSymmetric
    quickCheck $ forAll (arbitrary :: Gen [(Int, Int)])$ isTransitive
    quickCheck $ forAll (arbitrary :: Gen [(Int, Int)])$ subsetTransitive

{-
--------- LIBRARIES ---------

In this program, the following libraries are used:

- Exercise3: Where "symClos" function is implemented.
- Exercise5: Where "trClos" function is implemented.
- SetOrd: Type defination of Rel is defined in this module.
- Test.QuickCheck: To automatically test "symClos" and "trClos" functions.

--------- TESTING APPROACH ---------


This program is intended to test the functionality of functions "symClos" and "trClos" from Exercise3.hs and Exercise5.hs.
For this purpose, we automatically tested a number of different properties.
All properties are of type Bool, since they all use automatic generators to generate relevant test cases.

1. isSymmetric: This test checks for every tuple (x,y) in the result generated by symClos function whether there is also (y,x) in the result.
                This test returns True if the above symmetric property is valid and False otherwise. Symeric closures generated by symClos function
                are incorrect if this check fails.
2. subsetSymmetric: This test checks whether all elements in the input relation are also included in the symmetric closures.
3. isTransitive: This test checks for every tuple (x,y) in the result generated by trClos function, where (y,z) is a closure, whether (x,z) is also be a closure.
                 This test returns True if the above transitive property is valid and False otherwise. TRansitive closures generated by trClos function
                 are incorrect if this check fails.
4. subsetTransitive: This test checks whether all elements in the input relation are also included in the transitive closures.

As expected, all tests passed, giving the following output:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

This means that the implementation of the functions "symClos" and "trClos" is most likely correct.

-}