-- Study: MSc Software Engineering.
-- This program is intended to 
-- Time spent: 2 hours

module Exercise2 where

--Imports
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

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
setDifference (Set [])     set2  =  emptySet
setDifference (Set (x:xs)) set2
   | (inSet x set2) = setDifference (Set xs) set2
   | otherwise = insertSet x (setDifference (Set xs) set2)

--Intersection Properties

--All elements of intersection are in set1 and set2. 
prop_intersection :: Set Int -> Set Int -> Property
prop_intersection set1 set2 =
  let intersection = setIntersection set1 set2
  in counterexample ("set1: " ++ show set1 ++ "\nset2: " ++ show set2 ++ "\nintersection: " ++ show intersection) $
    property $ subSet intersection set1 && subSet intersection set2

--Union Properties

--All elements of set1 and set2 are in union.
prop_union :: Set Int -> Set Int -> Property
prop_union set1 set2 =
  let union = setUnion set1 set2
  in counterexample ("set1: " ++ show set1 ++ "\nset2: " ++ show set2 ++ "\nunion: " ++ show union) $
    property $ subSet set1 union && subSet set2 union

--Difference Properties

--Complement Property
--A-(A-B)=A∩B -> The set difference of A with the set difference of A and B is equivalent to the intersection of A and B.
prop_difference :: Set Int -> Set Int -> Property
prop_difference set1 set2 =
  let difference = setDifference set1 set2
      intersection = setIntersection set1 set2
  in counterexample ("set1: " ++ show set1 ++ "\nset2: " ++ show set2 ++ "\ndifference: " ++ show difference) $
          property $ (set2list difference) == (\\) (set2list set1) (set2list intersection)

main :: IO ()
main = do
    quickCheck prop_intersection
    quickCheck prop_union
    quickCheck prop_difference



