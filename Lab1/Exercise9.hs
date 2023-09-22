-- Study: MSc Software Engineering.
-- This program is intended to implement and test the "isPermutation" function
-- which checks whether two lists are permutations of each other.
-- Time spend: 6 hours

module Exercise9

where

import Test.QuickCheck
import Data.List

-- This function returns True if its arguments are permutations of each other
-- and False otherwise.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem y (permutations x)

-- This function returns a list with random numbers between 1 and 3.
-- This list will have maximum 3 elements and doesn't contain any duplicated element since
-- duplicated elements in the list will be removed.
listGen :: Gen [Integer]
listGen = do
    x <- choose (1, 3)
    y <- choose (1, 3)
    z <- choose (1, 3)
    return (nub [x, y, z])

-- As long as there is no duplicates in the list, the input lists are permutations
-- of each other if they satisfy the following properties:
-- 1. They have the same amount of elements.
prop_eq1 :: [Integer] -> [Integer] -> Property
prop_eq1 x y = isPermutation x y ==> (length x == length y)

-- 2. All elements in the first list are in the second list and vice versa.
-- If there is duplicates in the list, the second property will not be relevant since
-- for example two lists: [1, 2, 2] and [1, 1, 2] will still satisfy thid property while
-- they are not permutations of each other
prop_eq2 :: [Integer] -> [Integer] -> Property
prop_eq2 x y = isPermutation x y ==> (all (\item -> elem item x) y && all (\item -> elem item y) x)

-- 3. A list is always a permutation of itself.
prop_eq3 :: [Integer] -> Bool
prop_eq3 x = isPermutation x x == True

-- We automatically test the property using quickCheck and a list generator to generate inputs.
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

    quickCheck $ forAll listGen $ prop_eq3

{-
--------- LIBRARIES ---------

- Data.List
- Test.QuickCheck

For this program, the Gen construct, Property type, and the quickCheck function from
the Test.QuickCheck library are used. The permutations funciton from the Data.List library
is used for the isPermutation function.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program implements and tests the "isPermutation" function
swhich checks whether two lists are permutations of each other.

--------- TESTING APPROACH ---------

To test isPermutation function, we used the following properties:
As long as there is no duplicates in the list, the input lists are permutations
of each other if they satisfy the following properties:
1. They have the same amount of elements.
2. All elements in the first list are in the second list and vice versa.
   If there is duplicates in the list, the second property will not be relevant since
   for example two lists: [1, 2, 2] and [1, 1, 2] will still satisfy thid property while
   they are not permutations of each other
3. A list is always a permutation of itself.

These properties are used by quickCheck to test the my_subsequences function. For each property,
the precondition is defined before the '==>' operator to only allow the appropriate inputs,
inputs that don' satisfy the precondition will be discarded. The postcondition is defined after
'==>' operator, the test will only succeed if the postcondition is also satisfied.
To test this property, lists containing random numbers between 1 en 3 are used.
These generated list will have maximum 3 elements and doesn't contain any duplicated element.
The choise to limit the input list is to decrease the number of discarded tests.
How smaller the set

The quickCheck test passed, as expected, giving the following result:
+++ OK, passed 100 tests; 403 discarded.
+++ OK, passed 100 tests; 365 discarded.
+++ OK, passed 100 tests.

This means that the property described above is likely correct and proved by induction.

Is the property hard to test? If you find that it is, can you given a reason why?
- The property is hard to test, because the subsequences function takes a long time to
  execute when the input list has a length of 25 or higher. This is why I restricted the
  input n to be below 20, since this allows the tests to take <10 seconds.

When you perform the test for exercise 2, what are you testing actually?
Are you checking a mathematical fact? Or are you testing whether
subsequences satisfies a part of its specification? Or are you testing
something else still?
- When testing the for this exercise, we intent to simulate the proof of the property
  "If A is a finite set with |A| = n then |P(A)| = 2^n." by using induction.
  Base step with an empty set is tested, by testing 2^n and 2^(n+1), the induction step is also completed.

-}
