-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to implement and test the function to generate a list
-- of subsequences of a given list.
-- Time spend: 4 hours

module Exercise2 where

import Test.QuickCheck

-- This function generates a list of subsequences of a given list. This is done
-- using list comprehension and recursion (not for any perticular reason, this is
-- the only implementation I could think of).
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:y) = [x:z | z <- subsequences y] ++ subsequences y

-- For any list with n elements, the amount of subsequences should be 2^n.
prop :: Integer -> Property
prop n = (n >= 0) && (n < 20) ==> length (subsequences [1..n]) == 2^n

-- We automatically test the property using quickCheck.
main :: IO ()
main = do
    quickCheck prop


{-
--------- LIBRARIES ---------

For this program, the Gen construct, Property type, and the quickCheck function from
the Test.QuickCheck library are used.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program implements the subsequences function that generates a list of all subsequences
of a given list. This is done using list comprehension and recursion, since that is the only
implementation I could think of.

--------- TESTING APPROACH ---------

The subsequences function is used to test the following property:

"If A is a finite set with |A| = n then |P(A)| = 2^n."

To test this property, lists of the form [1..n] are used, with n being the input.
The precondition is that n should be greater than or equal to 0. I ended up using
a stronger precondition of (n >= 0) && (n < 20) for efficiency reasons.

The quickCheck test passed, as expected, giving the following output:
+++ OK, passed 100 tests; 428 discarded.

This means that the property described above is likely correct (not 100% proved).

Is the property hard to test? If you find that it is, can you given a reason why?

The property is hard to test, because the subsequences function takes a long time to
execute when the input list has a length of 25 or higher. This is why I restricted the
input n to be below 20, since this allows the tests to take <10 seconds.

When you perform the test for exercise 4, what are you testing actually?
Are you checking a mathematical fact? Or are you testing whether
subsequences satisfies a part of its specification? Or are you testing
something else still?



-}