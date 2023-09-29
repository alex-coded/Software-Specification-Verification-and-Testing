-- tau is a lable (same as epsilon), can be used in transition
-- after -> states after a lable. depth first searching, starting at the output
-- delta is recursive arrow
-- ! output
-- ? input
-- list of transitions and find the path
-- How to go from one state to another
-- subfunctions to trace the states.

-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent: 180 min
-- Additional dependencies
module Exercise4 where

import Data.List
import LTS
import Test.QuickCheck

-- nextTransitions':: [LabeledTransition]->State->[(State,Label)]
-- nextTransitions' lt q0 =  [(s',l) | (s,l,s')<- lt , s == q0]

-- findfollowingtransitions':: [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
-- findfollowingtransitions' lt st ls = [(s:st,ls++[l])| (s,l)<-nextTransitions' lt (head st)]

-- traces':: [LabeledTransition] -> [([State],[Label])]-> [([State],[Label])]
-- traces' lt [] = []
-- traces' lt pairs = pairs ++ traces' lt next
--     where next = concatMap (uncurry $ findfollowingtransitions' lt) pairs

-- traces :: LTS -> [Trace] -- [[Label]]
-- traces (q, l, lt, q0) = nub $ map snd (traces' lt [([q0],[])])

-- This function retrieves all s2 states in transitions (s1, label, s2).
nextStates':: [LabeledTransition]->[State]->Label->[State]
nextStates' lt state label = nub [s | (s', label, s)<- lt , elem s' state]

-- with every label retrive s2 states and check whether q0 is reached

after' :: [LabeledTransition] -> [State] -> Trace -> [State]
after' lt states [] = []
after' lt states (th:tt) = (nextStates' lt states th) ++ (after' lt (nextStates' lt states th) tt)

after :: IOLTS -> Trace -> [State]
after (_, _, _, _, q0) [] = [q0]
after (_, _, _, lt, q0) trace = nub (after' lt [q0] trace)



