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
6) Deterministic?

From Tretman:
1) Initial state q0 is in Q
2) Q is a countable, non-empty set of states
3) L is a countable set of labels
4) 

-}

-- Check if the LTS is well-formed
isWellFormed :: LTS.IOLTS -> Bool
isWellFormed iolts =
  let (allStates, allinputLabels, alloutputLabels, allTransitions, initial) = iolts
  in
    -- Check if all transitions have valid states
    all (\(from, _, to) -> from `elem` allStates && to `elem` allStates) allTransitions
    &&
    -- Check if the initial state is valid
    initial `elem` allStates



--Returns true iff a given IOLTS is valid
--validateLTS :: IOLTS -> Bool

-- Types
--type State = Integer
--type Label = String
--type LabeledTransition = (State, Label, State)
--type Trace = [Label]
--type LTS = ([State], [Label], [LabeledTransition], State)
--type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)