-- This program is intended to calculate the symmetric closure of a relation R
-- Time spent: 20 min

module Exercise3 where
import Data.List
import SetOrd

type Rel a = [(a, a)]

-- Symmetric closure
symClos :: Ord a => Rel a -> Rel a
symClos pairs = union pairs [(y, x) | (x, y) <- pairs]

-- Print symmetric closure of a relation of which the symmetric closure is known.
main :: IO ()
main = do
    let relation = [(1, 2), (2, 3), (3, 4)]
    print (list2set (symClos relation))


{-
--------- LIBRARIES ---------

For this program, the following lobraries are used:
- Data.List: Used to work on relations, which are lists of tuples.
- SetOrd: list2set is used to display relations in order.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to calculate the symmetric closure S of a given relation R. This is done ysing
the following definition of the transitive closure:

For any (a, b) in R, S should have a (b, a).

So in the symClos function, list comprehension is used to construct a list of (b, a) tuples for every
(a, b) in R. This list is unioned with the relation R. Union is used to avoid duplicate elements.

--------- TEST APPROACH ---------

The symClos function is tested in Exercise6.hs

-}

