-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent: 180 min
-- Additional dependencies
module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

-- This function returns true iff a given LTS is valid according
-- to thespecifications given in the Tretmans paper.
-- SPECIFICATIONS OF AN IOLTS 〈Q, Li ∪ Lu, T, q0〉 --
-- q is a countable, non-empty set of states
-- Both Li and Lu should be countable
-- q0 should be element of q
-- For every (q, μ, q') ∈ T, q and q' should be in q, and μ should be in (Li ∪ Lu ∪ {τ})
-- Li and Lu are countable sets of input labels and output labels and they should be disjoint: Li ∩ Lu = ∅
validateLTS :: IOLTS -> Bool
validateLTS (q, li, lu, t, q0) | (notCountable q) && (notCountable li) && (notCountable lu) = False
                               | q == [] = False
                               | not (elem q0 q) = False
                               | (intersect li lu) /= [] = False
                               | not ( validateTransitions t q (li ++ lu ++ [tau])) = False
                               | otherwise = True

validateTransitions :: [LabeledTransition] -> [State] -> [Label] -> Bool
validateTransitions [] q l = False
validateTransitions t q l = all (\ (s1, l1, s2) -> (elem s1 q) && (elem s2 q) && (elem l1 l)) t

-- This function returns True if a given list is not empty and False otherwise.
-- This extra function is needed to evaluate a output list from another function as a parameter.
isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty (_:_) = True

-- This function returns True is a list has more than 1000 elements and False otherwise.
-- We assume that those lists are infinite and thus not countable.
-- Function 'drop' drops n xs drops the first n elements off the front of the sequence xs.
-- We use isNotEmpty function to check if there is still elements in the
-- list after we dropped the first 1000 element of the list.
notCountable :: [a] -> Bool
notCountable lst = isNotEmpty $ drop 1000 lst

main :: IO ()
main = do
    print "haai"
