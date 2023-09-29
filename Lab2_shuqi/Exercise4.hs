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

nextStates':: [LabeledTransition]->[State]->Label->[(State, Label)]
nextStates' lt state label =  [(s',l) | (s',l,s)<- lt , elem s state, l == label]

findfollowingstates':: [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
findfollowingstates' lt states [] = []
findfollowingstates' lt states labels = nextStates' lt states (last labels)

traces'' :: LTS -> [([State],[Label])]
traces'' (q, l, lt, q0) = nub (traces' lt [([q0],[])])


after' :: [LabeledTransition] -> State -> [Label] -> [([State],[Label])]-> [([State],[Label])]
after' t q0 [] = []
after' t q0 (th:tt) = []

after :: IOLTS -> Trace -> [State]
after (_, _, _, _, q0) [] = q0
after (_, _, _, t, q0) (th:tt) =
after ((qh:qt), li, lu, t, q0) =



