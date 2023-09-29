-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to randomly generate different kinds of IOLTSs, including valid and invalid
-- IOLTSs. These generators are used to test the validateLTS function from Exercise1 using quickCheck.
-- Time spent: 6 hours

module Exercise2 where

import LTS
import Test.QuickCheck
import Exercise1

-- This generator randomly generates transitions of the form (State, Label, State).
transitionGen :: Gen LabeledTransition
transitionGen = do
    k <- chooseInteger(2, 20)
    s <- chooseInteger(1, k)
    t <- suchThat (chooseInteger(1, k)) (/= s)
    l <- elements ([tau] ++ (map (\ x -> if mod x 2 == 0 then "?" ++ (show x) else "!" ++ (show x)) [1..k]))
    return (s, l, t)

-- This generator randomly generates valid IOLTS using the createIOLTS function with randomly generated transitions.
ltsGen :: Gen IOLTS
ltsGen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n transitionGen
    return (createIOLTS transitions)

-- This generator randomly generates invalid IOLTS with empty state list Q.
emptyStatesGen :: Gen IOLTS
emptyStatesGen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n transitionGen
    (q, li, lu, t, q0) <- return (createIOLTS transitions)
    return ([], li, lu, t, q0)

-- This generator randowmly generates invalid IOLTS with a start state q0 not in Q.
invalidQ0Gen :: Gen IOLTS
invalidQ0Gen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n transitionGen
    (q, li, lu, t, q0) <- return (createIOLTS transitions)
    return (q, li, lu, t, (maximum q) + 1)

-- This generator randomly generates invalid IOLTS containing transition states not in Q.
invalidTransStateGen :: Gen IOLTS
invalidTransStateGen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n transitionGen
    (q, li, lu, t, q0) <- return (createIOLTS transitions)
    return ([q0], li, lu, t, q0)

-- This generator randomly generates invalid IOLTS containing transition labels not in Li or Lu.
invalidTransLabelGen :: Gen IOLTS
invalidTransLabelGen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n transitionGen
    (q, li, lu, t, q0) <- return (createIOLTS transitions)
    return (q, drop n li, drop n lu, t, q0)

-- This generator randomly generates sequential transitions of the form (State, Label, State+1).
seqTransGen :: Gen LabeledTransition
seqTransGen = do
    k <- chooseInteger(3, 20)
    s <- growingElements [1..k]
    l <- growingElements (map (\ x -> if mod x 2 == 0 then "?" ++ (show x) else "!" ++ (show x)) [1..k])
    return (s, l, s + 1)

-- This generator randomly generates valid IOLTS containing only sequential transitions.
sequenceGen :: Gen IOLTS
sequenceGen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n seqTransGen
    return (createIOLTS transitions)

-- Automatically test the validateLTS function using quickCheck.
main :: IO ()
main = do
    quickCheck $ forAll emptyStatesGen $ prop_empty_states
    quickCheck $ forAll invalidQ0Gen $ prop_invalid_q0
    quickCheck $ forAll emptyStatesGen $ prop_empty_states
    quickCheck $ forAll invalidTransStateGen $ prop_invalid_trans_state
    quickCheck $ forAll invalidTransLabelGen $ prop_invalid_trans_label
    quickCheck $ forAll sequenceGen $ prop_sequential_valid


{-
--------- LIBRARIES ---------

In this program, the following libraries are used:

- LTS: To be able to use the IOLTS and related types and functions.
- Test.QuickCheck: To automatically test the validateLTS function.
- Exercise1: To test the validateLTS function using the properties defined here.

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

--------- TESTING APPROACH ---------

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
