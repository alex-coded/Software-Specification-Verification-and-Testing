-- Study: MSc Software Engineering.
-- This program is intended to implement the function straces that returns all suspension traces of a given IOLTS.
-- Time spent: 180 min
-- Additional dependencies:

module Exercise3 where

import Data.List
import LTS
import Test.QuickCheck

-- This function returns True if a state in a list of state transitions is quiescent and False otherwise.
-- This function checks the State has an ouput label. If a state has an output label, this state is not a
-- quiescent and otherwise, this state is a quiescent.
isQuiescent :: [(State, Label)] -> [Label] -> Bool
isQuiescent nt lu = not (any (\lable -> elem lable (map snd nt)) lu)

-- This function finds suspension transitions, adds these transitions in the list of transitions stored in
-- IOLTS and returns the resulting transition list.
-- This function checks every state stored in the IOLTS whether the state is a quiescent,
-- if a state is a quiescent, a transition with delta label will be added to the list of
-- transitions with the quiescent being the input and the output state.
straces' :: IOLTS -> [LabeledTransition]
straces' ([], _, _, t, _) = nub t
straces' ((qh:qt), li, lu, t, q0) | isQuiescent (nextTransitions' t qh) lu = straces' (qt, li, lu, t ++ [(qh, delta, qh)], q0)
                                  | otherwise = straces' (qt, li, lu, t, q0)

-- This function returns all suspension traces of a given IOLTS.
--
straces :: IOLTS -> [Trace]
straces (q, li, lu, t, q0) = map snd (traces' (straces' (q, li, lu, t, q0)) [([q0],[])])
