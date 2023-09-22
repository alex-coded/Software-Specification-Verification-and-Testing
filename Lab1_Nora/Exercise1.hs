-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to implement and test the factorial function.
-- Time spent: 3 hours

module Exercise1 where

import Test.QuickCheck

-- This function takes an integer n and recursively calculates the factorial n!
-- The factorial of the edge case 0 is 1.
-- For n with a value greater than 0, this function will be executed recursively
-- to multiply all consecutive integers from 1 to n.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n - 1))

-- For a given natural number n, the factorial n! can be written as n * (n - 1)!.
-- Therefore, for all natural numbers n, n! / n should equal (n - 1)!.
prop_div :: Integer -> Bool
prop_div n = div (factorial n) n == factorial (n - 1)

-- The factorial of any natural number n should always be a positive number.
prop_pos :: Integer -> Bool
prop_pos n = factorial n > 0

-- The factorial of any natural number above 1, should always be even, since there
-- is always a 2 in the multiplication, which means it can always be divided by 2 as well.
prop_even :: Integer -> Bool
prop_even n = mod (factorial n) 2 == 0

-- The three factorial are automatically tested using quickCheck, and generators to
-- generate the inputs.
main :: IO ()
main = do
    quickCheck $ forAll (suchThat (arbitrary :: Gen Integer) (> 0)) $ prop_div
    quickCheck $ forAll (suchThat (arbitrary :: Gen Integer) (> 0)) $ prop_pos
    quickCheck $ forAll (suchThat (arbitrary :: Gen Integer) (> 1)) $ prop_even


{-
--------- LIBRARIES ---------

For this program, the Test.QuickCheck library is used to automatically test the factorial
function. From this library, the Gen construct, the Property type, and the quickCheck and
suchThat functions are used.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

The purpose of this program is to calculate the factorial n! of an integer input n.
This is done by recursively multiplying all consecutive integers from 1 to n.

--------- TESTING APPROACH ---------

To test the factorial function, the following three properties were tested:

1. division property: For any natural number n, the factorial n! can be written as
   n * (n - 1)!. This means that n! / n should equal (n - 1)!.
2. positive property: For any natural number n, the factorial n! should be positive.
3. even property: For any integer n above 1, the factorial n! should be even, since
   there is always a 2 in the multiplication. So n! should be divisible by 2.

These properties are used by quickCheck to test the factorial function. For each property,
a generator is used to generate the appropriate input. For the first two properties, the
input should be natural numbers, so integers > 0. For the third property, the input should
be integers > 1. The exercise requires a random generator. Since the quickCheck Gen construct
already generates random integers, the System.Random library is not needed. Since the exercise
calls for Gen construct generators, I did not need to use the Property type.

All three tests passed, as expected, giving the following output:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

This means that the implementation of the factorial function is most likely correct (not proven).

-}