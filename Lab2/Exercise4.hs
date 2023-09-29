-- Study: MSc Software Engineering.
-- This program is intended to implement and test the function after for IOLTS.
-- Time spent: too long

module Exercise4 where

--Imports
import Data.List
import LTS
import Test.QuickCheck
import Exercise2

{-

INFIX DEFINITION IN THE TRETMAN'S PAPER:

s after σ = {s' | s =σ⇒ s'}
the set of states reached after trace σ

Example:
Some states in which a system can be after a trace are: r after epsilon (nothing) = {r0};
r after but = {r1, r2}; r after but·choc = ∅ (empty set); v after but·liq·but = {v0, v1}

-}

-- This function retrieves all s2 states in transitions (s1, l, s2), , where l equals the given label.
nextStates':: [LabeledTransition] -> [State] -> Label -> [State]
nextStates' lt state label = nub [s | (s', l, s)<- lt , elem s' state, l == label]

-- This function recurces over labels a trace to find all states reached after that label.
--The final output is the states reached after a whole trace.
after' :: [LabeledTransition] -> [State] -> Trace -> [State]
after' lt states [] = states
after' lt states (th:tt) = after' lt (nextStates' lt states th) tt

--This function returns the set of states reached after the given trace.
after :: IOLTS -> Trace -> [State]
after (_, _, _, _, q0) [] = [q0]
after (_, _, _, lt, q0) trace = nub (after' lt [q0] trace)

-------------------------TESTING------------------------------------------------------

--Create tests to test the validity of your implementation with both traces and straces.

--------------------------PROPERTIES---------------------------------------------------

-- Reachability Property
--For every state s in the result of after iolts trace (for all traces in (straces iolts),
--there exists a trace from the initial state to s that matches the given trace.
--prop_reachability :: IOLTS -> Bool
--prop_reachability iolts =

--Identity Property:
--If the trace is empty, the result of after iolts []
--should be a list containing only the initial state.
prop_identity :: IOLTS -> Bool
prop_identity (q, li, lu, t, q0) = after (q, li, lu, t, q0) [] == [q0]

--Consistency with Single-Step Transitions:
--Compare the result of after with the result of applying a single labeled transition.
--For example, if after iolts [a] returns [s1, s2], check that there exist transitions [s0, a, s1] and [s0, a, s2] in the IOLTS.
--prop_consistency :: IOLTS -> Bool
--prop_consistency iolts =

-------------------------GENERATORS (from Exercise2)-----------------------------------------------------

-- This generator randomly generates transitions of the form (State, Label, State).
--transitionGen :: Gen LabeledTransition

-- This generator randomly generates valid IOLTS using the createIOLTS function with randomly generated transitions.
--ltsGen :: Gen IOLTS

main :: IO ()
main = do
    putStrLn "Reachability Property"
    --quickCheck $ forAll ltsGen $ prop_reachability
    putStrLn "Identity Property"
    --quickCheck $ forAll ltsGen $ prop_identity
    quickCheck prop_identity
    putStrLn "Consistency Property"
    --quickCheck $ forAll ltsGen $ prop_consistency

{-
--------- LIBRARIES ---------

In this program, the following libraries are used:

- Data.List: To perform list-specific actions lilke intersect.
- LTS: To be able to use the IOLTS and related types and functions.
- import Exercise2 - to access gprevious enerators.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

The function after is designed to return the set of states reached after the given trace.
To implement after, we use two auxiliary functions - nextStates' and after'.
The function nextStates' retrieves all s2 states from all transitions of form (s1, label, s2).
The function after' recurces over labels a trace to find all states reached after that label.
The function after then just 'launches' the process and removes duplicates from the final list.

--------- TESTING APPROACH -----------

To test the after function, we tested a number of different
properties. All properties are of type Bool, since they all use generators to generate relevant test
cases. The following properties were checked:

1. Reachability Property - for every state s in the result of after iolts trace,
there exists a trace from the initial state to s that matches the given trace.
2. Identity Property - if the trace is empty, the result of after iolts []
should be a list containing only the initial state.
3. Consistency with Single-Step Transitions - compare the result of after with the result of applying a single labeled transition.

I was only able to implement the identity property and test it using quickCheck. The tests passed.

quickCheck  prop_identity
-- +++ OK, passed 100 tests.

To test the other properties, I would use the following format:
--quickCheck $ forAll ltsGen $ prop_reachability

Traces and straces would be tested for as they would be included in the properties functions.

-}
