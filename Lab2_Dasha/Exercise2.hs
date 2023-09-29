-- Study: MSc Software Engineering.
-- This program is intended to implement generation for IOLTS and test function from Exercise 1.
-- Time spent: 

module Exercise2 where

--Imports
import Data.List
import LTS
import Test.QuickCheck
import Exercise1

-------------RANDOM GENERATORS---------------------------------------------

ltsGen :: Gen IOLTS
ltsGen = do
    k <- suchThat getSize (< 20)
    n <- choose(1, k)
    transitions <- vectorOf n transitionGen
    return (createIOLTS transitions)

------------FUNCTION TO GENERATE A TRANSITION-------------------------------

transitionGen :: Gen (State, Label, State)
transitionGen = do
    -- Generate a transition
    k <- chooseInteger(1, 20)
    s <- chooseInteger(1, k) -- State before transition
    t <- suchThat (chooseInteger(1, k)) (/= s) -- state after transition
    -- Choose 1 label from a list of labels
    l <- elements ([tau] ++ (map (\ x -> if mod x 2 == 0 then "?" ++ (show x) else "!" ++ (show x)) [1..k]))
    return (s, l, t)


-------quickCheck $ forAll ltsGen $ validateLTS



