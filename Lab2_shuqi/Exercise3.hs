-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent: 180 min
-- Additional dependencies

module Exercise3 where

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

-- straces' :: IOLTS -> [LabeledTransition]
-- straces' ([], _, _, t, _) = nub t
-- straces' (((s1, l, s2):qt), li, lu, t, q0) | (elem l li) = straces' (qt, li, lu, t ++ [(s1, delta, s1)], q0)
--                                            | (elem l lu) = straces' (qt, li, lu, t ++ [(s2, delta, s2)], q0)
--                                            | otherwise = straces' (qt, li, lu, t, q0)

isQuiescent :: [(State, Label)] -> [Label] -> Bool
isQuiescent labels lu = 

straces' :: IOLTS -> [LabeledTransition]
straces' ([], _, _, t, _) = nub t
straces' ((qh:qt), li, lu, t, q0) | isQuiescent (nextTransitions' t qh) = straces' (qt, li, lu, t ++ [(qh, delta, qh)], q0)
                                  | otherwise = straces' (qt, li, lu, t, q0)

straces :: IOLTS -> [Trace]
straces (q, li, lu, t, q0) = nub $ map snd (traces' (t ++ (straces' (q, li, lu, t, q0))) [([q0],[])])

