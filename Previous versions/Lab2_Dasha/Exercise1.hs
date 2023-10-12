-- Study: MSc Software Engineering.
-- This program is intended to write a function that tests validity of an LTS and test the properties of this function. 
-- Time spend: 1 hour

module Exercise1 where

--Imports
import Data.List
import LTS
import Test.QuickCheck

{-
A list of factors that result in invalid IOLTS's:

1) The initial state is not specified
2) Undefined/incorrect states of an LTS
3) Undefined/incorrect transitions of an LTS
4) Undefined/Incorrect labeling of transitions
5) Does not adhere to connectivity - there is not a path of transitions between any two states
-> If a state is unreachable, the IOLTS is invalid
6) IOlTS can be both Deterministic and non deterministic (?)

From Tretman:
1) Initial state q0 is in Q
2) Q is a countable, non-empty set of states
3) L is a countable set of labels
4) T ⊆ Q × (L ∪ {τ}) × Q, with τ /∈ L, is the transition relation;

-}

-- Checks if Initial state q0 is in Q (Tretman 1)
tretman1 :: LTS.IOLTS -> Bool
tretman1 iolts =
  let (allStates, allInputLabels, allOutputLabels, allTransitions, initialState) = iolts
  in
    -- Checks if initial state is in states
    initialState `elem` allStates

-- Checks transition relation (Tretman 4)
tretman4 :: LTS.IOLTS -> Bool
tretman4 iolts =
  let (allStates, allInputLabels, allOutputLabels, allTransitions, initialState) = iolts
  in
    -- Check if all transitions have valid states
    all (\(from, _, to) -> from `elem` allStates && to `elem` allStates) allTransitions
    --where all transitions are of tuple form (state, label, state)

--Checks if the list is countable
isCountable :: [a] -> Bool
isCountable xs = True

-- Checks if Q is a countable, non-empty set of states (Tretman 2)
tretman2 :: LTS.IOLTS -> Bool
tretman2 iolts =
  let (allStates, allInputLabels, allOutputLabels, allTransitions, initial) = iolts
  in
   -- Check if states set is non-empty
  not (null allStates)
  &&
   -- Check if states set is countable
  isCountable allStates
   
-- L is a countable set of labels (Tretman 3)
tretman3 :: LTS.IOLTS -> Bool
tretman3 iolts =
  let (allStates, allInputLabels, allOutputLabels, allTransitions, initial) = iolts
  in
   -- Check if labels set is countable
  isCountable allInputLabels && isCountable allOutputLabels

--Returns true iff a given IOLTS is valid
validateLTS :: IOLTS -> Bool
validateLTS iolts = tretman1 iolts && tretman2 iolts && tretman3 iolts && tretman4 iolts
