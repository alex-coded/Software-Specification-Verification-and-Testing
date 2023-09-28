module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck


validateLTS :: IOLTS -> Bool
validateLTS (q, li, lu, t, q0) | q == [] = False
                               | not (elem q0 q) = False
                               | not ( null ( intersect li lu)) = False
                               | not ( validateTransitions t q (li ++ lu ++ [tau])) = False
                               | otherwise = True

validateTransitions :: [LabeledTransition] -> [State] -> [Label] -> Bool
validateTransitions [] q l = False
validateTransitions t q l = all (\ (s1, l1, s2) -> (elem s1 q) && (elem s2 q) && (elem l1 l)) t

-- LIST OF LTS SPECIFICATIONS --
-- Q is countable and non-empty
-- Both Li and Lu should be countable
-- q0 should be element of Q
-- For every (q, μ, q') ∈ T, q and q' should be in Q, and μ should be in (Li ∪ Lu ∪ {τ})
-- Li and Lu should be disjoint: Li ∩ Lu = ∅