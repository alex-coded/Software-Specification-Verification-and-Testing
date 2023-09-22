-- Study: MSc Software Engineering.
-- This program is intended to implement and test the function to generate a list
-- of subsequences of a given list.
-- Time spend: 4 hours

module Exercise2

where

import Test.QuickCheck

-- This function generates a list of subsequences of a given list.
-- The edge case is when encounter an empty set, the result will be a list with an empty set.
-- When encounter a non-empty list, this function uses list comprehension and recursion to
-- retieve the subsequences.t
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (h:t) = (my_subsequences t) ++ [h:ps | ps <- my_subsequences t]

-- posGen :: Gen Integer
-- posGen = suchThat (arbitrary :: Gen Integer) (>= 1)

-- To prove that these statements are true, we use induction.
-- First step is the base step where we prove that the number of subsequences of an empty list
-- equals 2^0, which is 1.
prop1 :: Bool
prop1 = length (my_subsequences []) == 1

-- For any list with n elements, the amount of subsequences should be 2^n.
-- This property can be expressed in: If A is a finite set with |A| = n, then |P(A)| = 2^n.
prop2 :: Integer -> Property
prop2 n = (n >= 0) && (n < 20) ==> (length (my_subsequences [1..n]) == 2^n)

-- Induction step.
-- This property can be expressed in: If |P(A)| = 2^n, then |P(A)| * 2 = 2^(n+1).
prop3 :: Integer -> Property
prop3 n = (n >= 0) && (n < 20) ==> (length (my_subsequences [1..n]) * 2 == length (my_subsequences [1..(n + 1)]))

-- We automatically test the property using quickCheck.
main :: IO ()
main = do
    quickCheck prop1
    quickCheck prop2
    quickCheck prop3

{-
--------- LIBRARIES ---------

For this program, the Gen construct, Property type, and the quickCheck function from
the Test.QuickCheck library are used.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program implements the subsequences function that generates a list of all subsequences
of a given list. This is done using list comprehension and recursion, since that is the only
implementation I could think of.

--------- TESTING APPROACH ---------

The subsequences function is used to test the following properties:

"|P([])| is 1."
"If A is a finite set with |A| = n then |P(A)| = 2^n."
"If |P(A)| = 2^n, then |P(A)| * 2 = 2^(n+1)."

These properties are used by quickCheck to test the my_subsequences function. For each property,
the precondition is defined before the '==>' operator to only allow the appropriate inputs,
inputs that don' satisfy the precondition will be discarded. The postcondition is defined after
'==>' operator, the test will only succeed if the postcondition is also satisfied.
To test this property, lists of the form [1..n] are used, with n being the input.
The precondition is that n should be greater than or equal to 0. We ended up using
a stronger precondition of (n >= 0) && (n < 20) for efficiency reasons.

The quickCheck test passed, as expected, giving the following output:
+++ OK, passed 1 test.
+++ OK, passed 100 tests; 381 discarded.
+++ OK, passed 100 tests; 367 discarded.

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
