-- Study: MSc Software Engineering.
-- This program is intended to list and implement a number of different mutators.
-- Time spent: 2 hours

module Exercise1 where
import Test.QuickCheck

-- Shuffles list
shuffleList :: [Integer] -> Gen [Integer]
shuffleList xs = shuffle xs

-- Takes a random sublist of the output list.
subList :: [Integer] -> Gen [Integer]
subList xs = suchThat (sublistOf xs) (/= [])

-- Increments elements in list by 1.
incrementList :: [Integer] -> Gen [Integer]
incrementList xs = return (map (\x -> x + 1) xs)

-- Decrements elements in list by 1, if element is not 0.
decrementList :: [Integer] -> Gen [Integer]
decrementList xs = return (map (\x -> if x /= 0 then (x - 1) else x) xs)

-- Returns a list of a repeating random element, of a random length.
sameElements :: [Integer] -> Gen [Integer]
sameElements xs = do
             n <- choose(1, 20)
             m <- chooseInteger(1, 10)
             vectorOf n (elements [m])

customMutators = [shuffleList, subList, incrementList, decrementList, sameElements]

{-
--------- LIBRARIES ---------

In this program, the following library is used:
- Test.QuickCheck, from which the Gen construct and related functions are used to create generators.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

In Mutation.hs, three mutators are given: addElements, removeElements, and anyList. While these are
useful mutators, there are still a lot of other mutator options available. We came up with the following
list of other mutators:

1. Shuffle the output list.
2. Take a random sublist from the output list.
3. Increment the output list elements by 1.
4. Decrement the output list elements by 1, if the element is not 0.
5. Return a list containing only the same random element, of random length (e.g. [4, 4, 4, 4, 4]).

These 5 mutators were implemented in the code above. There is not much to say about the implementation.
For the first two mutators, the QuickCheck module has existing functions that do exactly that. For the
third and fourth mutators, the map function was used to increment or decrement every element in the
output list. Finally, for the fifth mutator, a random Int (list size) is chosen between 1 and 20, and
a random Integer (list element) is chosen between 1 and 10. The elements function is used to generate
only the chosen Integer element, and the vectorOf function is used to make it into a list of the chosen
size.

One key thing to avoid when creating a mutator is the generation of an empty list. When a mutant is
an empty list, an exception will be caused when using the mutator in the mutate and mutate' functions.

--------- TESTING APPROACH ---------

No testing required for this exercise.

-}
