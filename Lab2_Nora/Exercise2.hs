module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

transitionGen :: Gen (State, Label, State)
transitionGen = do
    -- Generate a transition
    k <- chooseInteger(1, 20)
    s <- chooseInteger(1, k) -- State before transition
    t <- suchThat (chooseInteger(1, k)) (/= s) -- state after transition
    -- Choose 1 label from a list of labels
    l <- elements ([tau] ++ (map (\ x -> if mod x 2 == 0 then "?" ++ (show x) else "!" ++ (show x)) [1..k]))
    return (s, l, t)

ltsGen :: Gen IOLTS
ltsGen = do
    k <- suchThat getSize (< 20)
    n <- choose(1, k)
    transitions <- vectorOf n transitionGen
    return (createIOLTS transitions)

