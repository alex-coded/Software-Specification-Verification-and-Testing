-- Study: MSc Software Engineering.
-- This program is intended to 
-- Time spent: 30 min

module Exercise2 where

--Imports
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

--Operation for set intersection
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set [])     set2  =  emptySet
setIntersection (Set (x:xs)) set2  
   | (inSet x set2) = insertSet x (setIntersection (Set xs) set2)
   | otherwise = setIntersection (Set xs) set2

--Operation for set union
setUnion :: Ord a => Set a -> Set a -> Set a 
setUnion (Set [])     set2  =  set2
setUnion (Set (x:xs)) set2  = 
   insertSet x (setUnion (Set xs) set2)

--Operation for set difference
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set [])     set2  =  set2
setDifference (Set (x:xs)) set2  
   | (inSet x set2) = setIntersection (Set xs) set2
   | otherwise = insertSet x (setIntersection (Set xs) set2)

main :: IO()
main = do
    quickCheck setIntersection
    quickCheck setUnion
    quickCheck setDifference





