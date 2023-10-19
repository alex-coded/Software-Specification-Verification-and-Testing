-- This program is intended to answer the following question:
-- "Is there a difference between the symmetric closure of the transitive closure of a relation R and
-- the transitive closure of the symmetric closure of R?"
-- Time spent: 60 min

module Exercise7 where
import Data.List
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Transitive Closure
trClos :: Ord a => Rel a -> Rel a
trClos r | (r @@ r) \\ r == [] = r
         | otherwise = trClos (union r (r @@ r))

-- Symmetric closure
symClos :: Ord a => Rel a -> Rel a
symClos pairs = union pairs [(y, x) | (x, y) <- pairs]

-- An example that illustrates the answer to the question (see comment below).
main :: IO ()
main = do
    let r = [(1, 2), (2, 3), (3, 4)]
    let s = [(1, 2), (2, 3), (3, 1)]
    let trsym = list2set (symClos (trClos r))
    let symtr = list2set (trClos (symClos r))
    let trsym2 = list2set (symClos (trClos s))
    let symtr2 = list2set (trClos (symClos s))
    putStr "Counterexample:\n"
    print (trsym == symtr)
    print trsym
    print symtr
    putStr "\nCyclic relation example:\n"
    print (trsym2 == symtr2)
    print trsym2
    print symtr2


{-
--------- LIBRARIES ---------

For this program, the following lobraries are used:
- Data.List: Used to work on relations, which are lists of tuples.
- SetOrd: list2set is used to display relations in order.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to answer the following question:
"Is there a difference between the symmetric closure of the transitive closure of a relation R and
the transitive closure of the symmetric closure of R?"

To answer this question, the symClos and trClos functions (and dependencies) from Exercise 3 and
Exercise 5 are copied here.

--------- ANSWER ---------

The answer to the question is: Yes, there is a difference.

This answer is illustrated by an example in the main function. We use the following relation as an
example:

R = [(1, 2), (2, 3), (3, 4)]

The symmetric closure of the transitive closure of R on domain [1, 2, 3, 4] is as follows:

S = {(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)}

The transitive closure of the symmetric closure of R on domain [1, 2, 3, 4] is as follows:

T = {(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)}

These are not equal and so there is a difference between the symmetric closure of the transitive
closure of a relation R and the transitive closure of the symmetric closure of R.

This makes sense for any relation that does not contain cycles. This is because the symmetric closure of
a relation R will create cycles for any number in the domain, since the opposite of any connection is added.
So for any (a, b) the opposite (b, a) is added. This means that the transitive closure of the symmetric
closure will always contain (a, a) and (b, b). But when first calculating the transitive closure of a
relation R that does not have cycles, there will be no tuples added of the form (a, a). So when then
calculating the symmetric closure of the transitive closure, there will also be no elements of the form
(a, a) added, since there were none in the transitive closure. This already illustrates a difference
between the symmetric closure of the transitive closure and the transitive closure of the symmetric
closure. This can also be seen in the example above.

When the relation R forms a cycle, both will be equal. This is shown in the second example with the
following relation:

R = [(1, 2), (2, 3), (3, 1)]

The symmetric closure of the transitive closure of R on domain [1, 2, 3] is as follows:

S = {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

The transitive closure of the symmetric closure of R on domain [1, 2, 3] is as follows:

T = {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

As predicted, these are equal.

In conclusion: Yes, in general there is a difference. One exception is when the relation R is cyclical,
then both are equal.

-}