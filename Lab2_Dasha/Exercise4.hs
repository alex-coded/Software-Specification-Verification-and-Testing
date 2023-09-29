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

--function i wrote
--Implements the function after (infix) for IOLTS corresponding with the definition in the Tretmans paper. 
after :: State -> Trace -> Maybe [State] --can be called using infix notation.
after state [] = Just [state] --state reached after epsilon (nothing).
after state (label:traces)  = --traversing the trace.
        case transition state label of
        Just nextState -> after nextState traces
        Nothing -> Nothing

--function i was trynna figure out
-- Perform a transition in the IOLTS
transition :: State -> Label -> Maybe State
transition state label = Just state

--Their specification
after :: IOLTS -> Trace -> [State]

-------------------TESTS-----------------------------------------