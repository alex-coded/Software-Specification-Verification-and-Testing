-- Study: MSc Software Engineering.
-- This program is intended to implement generation for IOLTS and test function from Exercise 1.
-- Time spend: 1 hour

module Exercise2 where

--Imports
import Data.List
import LTS
import Test.QuickCheck:q

-------------RANDOM GENERATORS--------------------------------------

--A random generator for LTS.
--ltsGen :: Gen IOlTS
--ltsGen = do

--------------FUNCTION TO GENERATE LISTS OF STATES AND LABELS-----------

listState :: Gen (State, [State], [Label])
listState = do
    -- Generate states and starting state size
    k <- choose (1, 19)
    let InitState = 1

    --Generate list of states
    StatesList <- vectorOf k (arbitrary :: Gen State)

    -- Generate labels size
    k <- choose (1, 19)

    --Generate list of labels
    let li = ["?" ++ show x | x <- [1..k']]
        lu = ["!" ++ show x | x <- [1..k']]
    LabelsList = li + lu

    return (InitState, StatesList, LabelsList)

--------------FUNCTION TO GENERATE A LIST OF TRANSITIONS-----------------

-- Define a transition type
type Transition = (state, label, state)

--Transition function - given a state and a label, generates the next state
transitionFunction :: Integer -> Integer -> Integer
transitionFunction currentState label = 1
transitionFunction currentState _ = currentState  -- No transition for other actions

-- Generate a list of transitions
generateTransitions :: [state], [label], (state -> label -> state)-> [Transition] 
--Inputs:
--list of states
--list of labels
--transitionFunction
generateTransitions states labels transitionFunc =
  [ (fromState, label, toState) | fromState <- states, label <- labels, let toState = transitionFunc fromState label]

