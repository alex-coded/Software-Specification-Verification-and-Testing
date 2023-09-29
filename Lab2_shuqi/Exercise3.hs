-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent: 180 min
-- Additional dependencies

module Exercise3 where

import Data.List
import LTS
import Test.QuickCheck

-- This function returns True if a state in a list of state transitions is quiescent and False otherwise.
isQuiescent :: [(State, Label)] -> [Label] -> Bool
isQuiescent nt lu = not (any (\lable -> elem lable (map snd nt)) lu)

straces' :: IOLTS -> [LabeledTransition]
straces' ([], _, _, t, _) = nub t
straces' ((qh:qt), li, lu, t, q0) | isQuiescent (nextTransitions' t qh) lu = straces' (qt, li, lu, t ++ [(qh, delta, qh)], q0)
                                  | otherwise = straces' (qt, li, lu, t, q0)

straces :: IOLTS -> [Trace]
straces (q, li, lu, t, q0) = nub $ map snd (traces' (t ++ (straces' (q, li, lu, t, q0))) [([q0],[])])
