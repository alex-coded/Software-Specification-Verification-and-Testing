-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to check if one list is a derangement of another. This
-- functionality is tested using quickCheck on multiple properties.
-- Time spent: 14 hours

module Exercise4 where

import Data.List
import Test.QuickCheck

-- This function checks if one list is a derangement of the other list by looking
-- at whether both lists contain the same elements and whether one element is not
-- on the same index in both lists. This function assumes there are no duplicates
-- in either lists.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] x = False
isDerangement x y | length x /= length y = False
                  | otherwise = all (\ z -> (elem z x) && (elem z y) && ((elemIndex z x) /= (elemIndex z y))) x

-- This function creates a list of all derangements of a list [0..n-1] by generating
-- the list of all permutations and filtering out all lists that are not derangements.
deran:: Int -> [[Int]]
deran n = filter (\ x -> isDerangement [0..n-1] x) (permutations [0..n-1])

-- This function does the opposite of the function above. It filters out the derangements
-- from the list of permutations of the list [0..n-1]. The result is a list of permutations
-- that are not derangements.
nonderan:: Int -> [[Int]]
nonderan n = filter (\ x -> (isDerangement [0..n-1] x) == False) (permutations [0..n-1])

-- The purpose of this generator is to generate integers between 1 and 7. This generator
-- is used to test the second and third properties. See comment report below for further reasoning.
limitedSizeGen :: Gen Int
limitedSizeGen = suchThat (arbitrary :: Gen Int) (\ x -> (x > 1) && (x < 7))

-- Since the empty list is a derangement of itself, the result of isDerangement
-- on two empty lists should always be true.
prop_empty :: Bool
prop_empty = isDerangement ([]::[Int]) ([]::[Int]) == True

-- Two lists containing only one element can never be a derangement of eachother so
-- the result of the isDerangement function of two singleton lists should always be false.
prop_single :: Int -> Int -> Bool
prop_single x y = isDerangement [x :: Int] [y :: Int] == False

-- The function deran generates all derangements of a list [0..n-1]. So the result of
-- the isDerangement function on the list [0.n-1] and any list from the deran output,
-- should always be true.
prop_deran :: Int -> Bool
prop_deran n = all (\ x -> isDerangement [0..n-1] x) (deran n)

-- The function nonderan generates all permutations that are not derangements. So the
-- result of the isDerangement function on the list [0..n-1] and any list from the nonderan
-- output, should always be false.
prop_nonderan :: Int -> Bool
prop_nonderan n = all (\ x -> (isDerangement [0..n-1] x) == False) (nonderan n)

-- Any even list is a derangement of its reverse. So the result of the isDerangement on
-- a even list and its reverse should always be true.
prop_reverse :: Int -> Property
prop_reverse n = (n >= 2) && (mod n 2 == 0) ==> isDerangement [0..n-1] (reverse [0..n-1]) == True

-- This is a dummy Bool version of the reverse property for the purpose of comparing
-- strength with other properties.
dummy_prop_reverse :: Int -> Bool
dummy_prop_reverse n = isDerangement [0..n-1] (reverse [0..n-1]) == True

-- A list can never be a derangement of itself. So the result of the isDerangement
-- function on two identical lists, should always be false.
prop_same :: Int -> Property
prop_same n = (n >= 2) ==> isDerangement [0..n-1] [0..n-1] == False

-- This is a dummy Bool version of the same property for the purpose of comparing
-- strength with other properties.
dummy_prop_same :: Int -> Bool
dummy_prop_same n = isDerangement [0..n-1] [0..n-1] == False

infix 1 -->

-- This function is a helper function for the stronger and weaker functions.
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- This function is a helper function for the stronger and weaker functions.
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Functions used to determine which of two properties is stronger or weaker.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

-- Use quickCheck to automatically test the aforementioned properties of isDerangement.
-- For each property, an appropriate quickCheck generator is used (explained in report below).
main :: IO ()
main = do
    quickCheck prop_empty
    quickCheck prop_single
    quickCheck $ forAll limitedSizeGen $ prop_deran
    quickCheck $ forAll limitedSizeGen $ prop_nonderan
    quickCheck prop_reverse
    quickCheck prop_same


{-
--------- LIBRARIES ---------

This program uses the following libraries:

- Data.List
- Test.QuickCheck

The elemList funciton from the Data.List library is used for the isDerangement function.
The Gen construct and the quickCheck, getSize, and suchThat functions from the Test.QuickCheck
library are used to test the implementation of the isDerangement funciton.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

The purpose of this code is to:

1. Determine whether one list is a derangement of another one (isDerangement)
2. Generate the list of all derangements of the list [0..n-1] (deran)
3. Test the isDerangement function using a list of properties

Step 1 is done by defining a function that takes two lists and checks whether all elements
in list 1 are also in list 2 and vica versa, and whether no two elements on the same index
are equal. If the lengths of the lists are not equal, False is returned. For this implementation
to be correct, I assume there are no duplicates in either list. I chose to do this because it
would make the function, and therefore the testing, a lot slower if I had to account for duplicates.
To account for duplicates, I would have checked if list 2 was an element in the list of permutations
of list 1, and if so, whether no element on the same index in both lists is equal.

Step 2 is done by defining a function that takes an integer n and uses the permutations function to
generate the list of permutations of the list [0..n-1]. From those permutations, all elements that are
not considered derangements by the isDerangement function are filtered out, leaving us with a list of
only derangements.

--------- TESTING APPROACH ---------

Step 3 is done by creating a list of properties of the isDerangement function and testing
these properties using quickCheck. The following properties were tested:

1. (Base case property) empty list: An empty list is a derangement of itself.
2. (Base case property) singleton list: Two lists containing 1 element can never be
   derangements of each other.
3. known derangements: When inputting two lists that are known to be derangements of each
   other, the isDerangement function should always return True.
4. known non-derangements: When inputting two lists that are known to not be derangements
   of each other, the isDerangement function should always return False.
5. reverse lists: An even list and its reverse are always derangements of each other.
6. same lists: A list of 1 or more elements is never a derangement of itself.

For the first property, no generator is needed, since there is only one input. It is also non
needed to use the quickCheck function, but it is the easiest way to show that the test passes with
IO () as output type. For the second property, there is no precondition for the input, so no
generator or Property type is needed. For properties 3, 4, 5 and 6, lists of the form [0..n-1] are
used to test the properties, and there are preconditions to the input n. In case of property 3 and 4,
a size generator is used to generate the input size n. This size generator is limited to be between
2 and 7. We chose to limit the size to >= 2 because lists of size 0 and 1 are already tested in
properties 1 and 2. The upper bound of 7 is chosen for efficiency reasons, since the permutations
function (used by deran and nonderan) takes a long time on lists larger than 6. I could have used
the Property type and define the preconditio in it, but since this precondition is so strong, too
many tests get discarded, and quickCheck gives up on the test after 1000 discarded tests. So instead,
I used the size generator. For properties 5 and 6, I did use the Property type, and defined the pre-
condition in the property. Here, the discarded tests are not a problem since there is no upper bound
in the precondition.

As for the strength of the properties, only the last 4 properties can be compared to eachother. I did
try to compare property strength using the stronger and weaker functions. However, this was difficult
since the deran and nonderan functions take very long to execute for an input size that exceeds 7. Using
such a small domain is not ideal for the prop_same and prop_reverse properties. So I decided to compare
property strength myself, and I ended on the following ranking (from strongest to weakest):

1. Known derangements and known non-derangements (same strength)
2. Reverse lists
3. Same lists

The postconditions of all these properties have the same strength, so I compared the strength
of the preconditions. Known derangements and known non-derangements have the same, strongest,
precondition of (n > 1) && (n < 7). Reverse lists has a weaker precondition of (n >= 2) &&
(mod n 2 == 0). Same lists has the weakest precondition of (n >= 2).

I do wonder if properties 3 and 4 are valid for testing the isDerangement funciton in their
current implementation, since the properties use the deran and nonderan functions. In their
definition, these functions already assume that isDerangement works correctly.

Can you automate the test process?

Yes, I use quickCheck to perform automated testing of the 6 properties.

All quickCheck tests pass, as expected, giving the following output:
+++ OK, passed 1 test.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests; 375 discarded.
+++ OK, passed 100 tests; 149 discarded.

This means that my implementation of isDerangement is most likely correct (not 100% proved).

-}