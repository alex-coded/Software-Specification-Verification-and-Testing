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
setDifference set1     (Set [])  =  set1
setDifference (Set (x:xs)) (Set(y:ys))
   | (inSet x (Set(y:ys)))  = setDifference (Set xs) (Set ys)
   | (inSet y (Set(x:xs))) = setDifference (Set xs) (Set ys)
   | not ((inSet x (Set(y:ys)))) = insertSet x (setDifference (Set xs) (Set (y:ys)))
   | not ((inSet y (Set(x:xs)))) = insertSet y (setDifference (Set (x:xs)) (Set ys))

prop_intersection :: Set Int -> Set Int -> Bool
prop_intersection set1 set2 =
    let result = setIntersection set1 set2
    in all (\x -> inSet x result) set1 && all (\x -> inSet x result) set2

prop_union :: Set Int -> Set Int -> Bool
prop_union set1 set2 =
    let result = setUnion set1 set2
    in all (\x -> inSet x result) set1 && all (\x -> inSet x result) set2

prop_difference :: Set Int -> Set Int -> Bool
prop_difference set1 set2 =
    let result = setDifference set1 set2
    in (all (\x -> inSet x result) set1 && all (\x -> not (inSet x result)) set2) 
    && (all (\y -> not (inSet y result)) set1 && all (\y -> inSet y result) set2)


main :: IO()
main = do
    quickCheck prop_intersection
    quickCheck prop_union
    quickCheck prop_difference




