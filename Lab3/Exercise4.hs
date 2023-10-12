-- Study: MSc Software Engineering.
-- This program is intended to calculate the strength of a set of properties.
-- Time spent: 2 hours

module Exercise4 where
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Exercise2 (countSurvivors)

-- This function counts the number of killed mutants.
countKilled :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countKilled n prop fun mutator = do
            survivors <- countSurvivors n prop fun mutator
            return (n - survivors)

-- This function calculates the percentage of killed mutants.
strengthPerc :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
strengthPerc n prop fun mutator | n == 0 = return (0)
                                | otherwise = do
                                    killed <- countKilled n prop fun mutator
                                    return ((div killed n) * 100)

-- Do a simple check to see if the strength of all multiplicationTable properties is correct.
main :: IO Int
main = do
    strengthPerc 4000 multiplicationTableProps multiplicationTable addElements


{-
--------- LIBRARIES ---------

This program uses the following libraries:
- Test.QuickCheck: To be able to use the Gen construct for the mutators.
- Mutation: To be able to use the standard mutators.
- MultiplicationTable: To be able to use the properties and function for checking.
- Exercise 2 (countSurvivors): Access to the countSurvivors function.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to calculate the strength of a set of properties. The strength is defined as
the percentage of killed mutations. So we need to count the number of failed mutants and compare it with
the total number of mutants. I implemented this by counting the number of survivors
(using the countSurvivors function) and subtracting that from the total number of mutants in order to
get the number of killed mutants. These killed mutants include mutants that did not actually change the
output, which is correct according to my classification described in Exercise2.hs. When I have the number
of killed mutants, I divide this mumber by the total number of mutants and multiply by 100, to get the
percentage of killed mutants.

--------- IMPORTANCE OF STRENGTH ---------

The strength of a set of properties is important information, because this tells us something about the
conjectures that this property set might be involved in. We know that conjectures are not very likely to
hold when the killed percentage is either 0 or 100. This makes sense, since this is clearly not a very
specific percentage, any mutator that coincidentally does not affect two aspects of the output that two
properties are testing, will result in a conjecture when this might just be a coincidence. For example,
when looking at the output from the countSurvivors function from Exercise2, when using the shuffleList
mutator, the percentage of survivors for both the prop_tenElements and the prop_moduloIsZero properties
is 100% (so 0% killed for both). However, these two properties do not imply one another. This result is
just a result of the shuffleList mutator not altering the length and the elements themselves. The same
logic goes for two properties with 100% killed mutants. So the strength of the conjectures in these cases
is not high. It is logical to assume that a conjecture is the strongest when the killed percentage is exactly
in between 0% and 100%, so 50%. The FitSpec paper proposes the following conjecture strength labels on a
scale of killed percentage:

  Weak      Mild           Strong           Mild      Weak
|------|------------|------------------|------------|------|
0%    11%          33%                66%          88%    100%

It is important to know what conjectures are likely to hold, in order to construct the most minimal and
complete set of properties that describes the function under testing.

-}
