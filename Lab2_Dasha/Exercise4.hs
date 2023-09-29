-- Study: MSc Software Engineering.
-- This program is intended to implement and test the function after for IOLTS. 
-- Time spent: 1h

module Exercise4 where

--Imports
import Data.List
import LTS
import Test.QuickCheck

{-

INFIX DEFINITION IN THE TRETMAN'S PAPER:

s after σ = {s' | s =σ⇒ s'}
the set of states reached after trace σ

Example:
Some states in which a system can be after a trace are: r after epsilon (nothing) = {r0};
r after but = {r1, r2}; r after but·choc = ∅ (empty set); v after but·liq·but = {v0, v1}

-}

-- This function retrieves all s2 states in transitions (s1, label, s2).
-- This function retrieves all s2 states in transitions (s1, label, s2).
nextStates':: [LabeledTransition] -> [State] -> Label -> [State]
nextStates' lt state label = nub [s | (s', label, s)<- lt , elem s' state]

-- with every label retrive s2 states and check whether q0 is reached

after' :: [LabeledTransition] -> [State] -> Trace -> [State]
after' lt states [] = states
after' lt states (th:tt) = after' lt (nextStates' lt states th) tt

after :: IOLTS -> Trace -> [State]
after (_, _, _, _, q0) [] = [q0]
after (_, _, _, lt, q0) trace = nub (after' lt [q0] trace)

-------------------------TESTING------------------------------------------------------

--Create tests to test the validity of your implementation with both traces and straces.

--------------------------PROPERTIES---------------------------------------------------

-- Property: Reachability Property
--For every state s in the result of after iolts trace,
--there exists a trace from the initial state to s that matches the given trace.
--prop_reachability :: IOLTS -> Property
--prop_reachability iolts 


-------------------------GENERATORS (from Exercise2)-----------------------------------------------------

-- This generator randomly generates transitions of the form (State, Label, State).
--transitionGen :: Gen LabeledTransition

-- This generator randomly generates valid IOLTS using the createIOLTS function with randomly generated transitions.
--ltsGen :: Gen IOLTS

main :: IO ()
main = do
    putStrLn "Property 1: Suspended traces must not contain quiescent states."
    --quickCheck $ forAll ltsGen $ prop_no_quiescent_states
    