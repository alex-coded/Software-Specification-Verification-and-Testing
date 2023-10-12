-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to validate an IOLTS using the validateLTS function, and to define
-- properties to test the validateLTS function.
-- Time spent: 4 hours

module Exercise1 where

import Data.List
import LTS

{-
--------- INVALIDATING FACTORS ---------

1. The list of states (Q) should be non-empty.
2. Start state (q0) should be element of Q.
3. The list of input labels (Li) and the list of output labels (Lu) should be disjoint: Li ∩ Lu = ∅.
4. Q should be countable.
5. Both Li and Lu should be countable.
6. For every transition (q, μ, q') ∈ T, q and q' should be in Q, and μ should be in (Li ∪ Lu ∪ {τ})

-}

-- Validate an IOLTS using the list of factors mentioned above.
validateLTS :: IOLTS -> Bool
validateLTS (q, li, lu, t, q0) | q == [] = False
                               | not (elem q0 q) = False
                               | not (null (intersect li lu)) = False
                               | not (isCountable 1000 q) = False
                               | not (isCountable 1000 (li ++ lu)) = False
                               | not (validateTransitions t q (li ++ lu ++ [tau])) = False
                               | otherwise = True

-- This function checks whether all states and labels found in the transitions are part of the
-- list of states Q and the lists of labels Li, Lu and tau.
validateTransitions :: [LabeledTransition] -> [State] -> [Label] -> Bool
validateTransitions [] q l = False
validateTransitions t q l = all (\ (s1, l1, s2) -> (elem s1 q) && (elem s2 q) && (elem l1 l)) t

-- This function takes a limit and checks whether a list is larger than the limit. If it is, true
-- is returned. If not, false is returned and the list is treated as infinite.
isCountable :: Eq a => Int -> [a] -> Bool
isCountable limit x | drop limit x == [] = True
                    | otherwise = False

-- An IOLTS with an empty state list Q is always invalid.
prop_empty_states :: IOLTS -> Bool
prop_empty_states ([], li, lu, t, q0) = validateLTS ([], li, lu, t, q0) == False

-- An IOLTS with a start state q0 that is not in Q is always invalid.
prop_invalid_q0 :: IOLTS -> Bool
prop_invalid_q0 (q, li, lu, t, q0) = (not (elem q0 q)) && (validateLTS (q, li, lu, t, q0) == False)

-- An IOLTS containing a state in a transition that is not part of Q is always invalid.
prop_invalid_trans_state :: IOLTS -> Bool
prop_invalid_trans_state (q, li, lu, t, q0) = (not (all (\ (s, _, t) -> (elem s q) && (elem t q)) t)) && validateLTS (q, li, lu, t, q0) == False

-- An IOLTS containing a label in a transition that is not in Li, Lu or tau is always invalid.
prop_invalid_trans_label :: IOLTS -> Bool
prop_invalid_trans_label (q, li, lu, t, q0) = (not (all (\ (_, l, _) -> (elem l li) || (elem l lu)) t)) && validateLTS (q, li, lu, t, q0) == False

-- A random IOLTS generated with createIOLTS is always valid.
prop_random_valid :: IOLTS -> Bool
prop_random_valid iolts = validateLTS iolts == True

-- A IOLTS containing only sequential transitions are always valid.
prop_sequential_valid :: IOLTS -> Bool
prop_sequential_valid iolts = validateLTS iolts == True

-- Empty main method in case that is required for sumbission.
main :: IO ()
main = putStr ""


{-
--------- LIBRARIES ---------

In this program, the following libraries are used:

- Data.List: To perform list-specific actions lilke intersect.
- LTS: To be able to use the IOLTS and related types and functions.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to validate a IOLTS. This is done using the validating factors at the top
of this file. Each factor is individually checked, and if all factors are correct for an IOLTS, it is
valid and validateLTS returns True. If any factors are incorrect, validateLTS returns false.

Helper functions are defined to check factors 4, 5 and 6. In order to check countability of a set,
we check whether it is infinite or not. If it is infinite, it is not countable and vica versa. To
check infinity of a set, we cannot use the length function, since this will go on forever if a set
is infinite. Therefore, we set a limit and check whether the set is greater than that limit. If it
is greater than that limit, we view it as infinite.

--------- TESTING APPROACH ---------

This is the copied test-report from the tests performed in Exercise2.

To test the implementation of the validateLTS function, we automatically tested a number of different
properties. All properties are of type Bool, since they all use generators to generate relevant test
cases. The following properties were checked:

1. An IOLTS with an empty state list Q is always invalid.
2. An IOLTS with a start state q0 that is not in the state list Q, is always invalid.
3. An IOLTS that contains a state in a transition that is not in the state list Q, is always invalid.
4. An IOLTS that contains a label in a transition that is not in the label lists (Li + Lu + tau), is always invalid.
5. An IOLTS randomly generated using the createIOLTS function is always valid.
6. An IOLTS that contains only sequential transitions, is always valid.

Each property needs its own generator to generate relevant testcases, see Exercise2.hs for the
explanation of all the generators.

As expected, all tests passed, giving the following output:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

This means that the implementation of validateLTS is most likely correct.

-}
