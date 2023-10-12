-- This program is intended to calculate the transitive closure of a relation R.
-- Time spent: 120 min

module Exercise5 where
import Data.List
import SetOrd

type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Transitive Closure
trClos :: Ord a => Rel a -> Rel a
trClos r | (r @@ r) \\ r == [] = r
         | otherwise = trClos (union r (r @@ r))

-- Print transitive closure of a relation of which the transitive closure is known.
main :: IO ()
main = do
    let relation = [(1, 2), (2, 3), (3, 4)]
    print (list2set (trClos relation))


{-
--------- LIBRARIES ---------

For this program, the following lobraries are used:
- Data.List: Used to work on relations, which are lists of tuples.
- SetOrd: list2set is used to display relations in order.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to calculate the transitive closure T of a given relation R. This is done ysing
the following definition of the transitive closure:

T = ∪ Ri for 1 <= i <= ∞
where
R1 = R
R(i+1) = R @@ Ri

So in the trClos function, the @@ operator is applied to R and itself, and the outcome is unioned with
R itself. This process is recursively done until no more elements can be unioned with R.

--------- TEST APPROACH ---------

The trClos function is tested in Exercise6.hs

-}