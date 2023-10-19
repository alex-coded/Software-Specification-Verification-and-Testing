module Exercise3 where

import SetOrd
import Data.List

type Rel a = [(a,a)]

-- Symmetric closure
symClos :: Ord a => Rel a -> Rel a
symClos r = nub (r ++ [(y, x) | (x, y) <- r])

{-
--------- LIBRARIES ---------

In this program, the following libraries are used:

- Data.List: nub function of this library is used to remove duplicated elements in a list.
--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to generate different kinds of IOLTSs. The main generators in this program are:

1. ltsGen: randomly generates valid IOLTSs by using the transitionGen generator
2. emptyStatesGen: randomly generates invalid IOLTSs with empty state list Q.
3. invalidQ0Gen: randomly generates invalid IOLTSs with start states q0 not in Q.
4. invalidTransStateGen: randomly generates invalid IOLTSs containing states in transitions that are
   not present in Q.
5. invalidTransLabelGen: randomly generates invalid IOLTSs containing labels in transitions that are
   not present in Li or Lu.
6. sequenceGen: randomly generates valid IOLTSs containing only sequential transitions using the
   seqTransGen generator.

All of these generators are implemented in the same way:
There is a transition generator that randomly generates two states and a label to form a random transition
of the form (State, Label, State). In the actual generator, an upper bound k is randomly chosen between
2 and 20. We chose to limit this at 20 to keep the tests efficient. Then, the number of transitions is
randomly chosen between 2 and k. We then randomly generate the list of n transitions using the vectorOf
function and the transition generator. Finally, the IOLTS is generated using the createIOLTS function and
the generated list of n transitions. When relevant, the resulting IOLTS is stored and altered as called
for by the generator constraints.
-}
