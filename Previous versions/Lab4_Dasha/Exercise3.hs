-- Study: MSc Software Engineering.
-- This program is intended to 
-- Time spent: 30 min

module Exercise3 where

--Imports
import Data.List
import System.Random
import Test.QuickCheck

--Relation is represented as an ordered list of pairs 
type Rel a = [(a,a)]

--Gives a symmetric closure of a relation 
symClos :: Ord a => Rel a -> Rel a


--symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
