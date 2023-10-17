-- Study: MSc Software Engineering.
-- This program is intended to implement operations for set intersection, set union and set difference.
-- Time spent: 2 hours

module Exercise2 where

--Imports
import Test.QuickCheck
import SetOrd
import Data.List
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

--All elements of intersection are in set1 and set2 -> intersection is a subset of set1 and set2. 
prop_intersection :: Set Int -> Set Int -> Property
prop_intersection set1 set2 =
  let intersection = setIntersection set1 set2
  in counterexample ("set1: " ++ show set1 ++ "\nset2: " ++ show set2 ++ "\nintersection: " ++ show intersection) $
    property $ subSet intersection set1 && subSet intersection set2

--Union Properties

--All elements of set1 and set2 are in union - set1 and set2 are a subset of union. 
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

{-
--------- LIBRARIES ---------

This program uses the following libraries:
- Test.QuickCheck: To be able to use the Gen construct for testing with generators. 
- SetOrd: For the Set datatype and its relevant functions. 
- Exercise 1: Access to our own generator. 
- Data.List: To be able to use the (//) double backslash list difference operator.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to implement operations for set intersection, set union and set difference.
First, I extract the implementation for set union directly from SetOrd (copy it rather than import for visual aid).

--------- TEST APPROACH ---------

To test the implementation of the set operators, we use automatic testing on a number of different
properties. All properties are of type Bool, since they all use generators to generate relevant test
cases. We first tested using our own generator from Exercise1, then QuickCheck. 

The following properties were checked:

--Intersection Properties

1. All elements of intersection are in set1 and set2 -> intersection is a subset of set1 and set2. 
This seemed intuitive to test upon seeing the subset function in SetOrd. 

--Union Properties

1. All elements of set1 and set2 are in union - set1 and set2 are a subset of union. 
Same as above. 

--Difference Properties

1. Complement Property -> A-(A-B)=A∩B
The set difference of A with the set difference of A and B is equivalent to the intersection of A and B.

Each property needs its own generator to generate relevant testcases, see Exercise1.hs for the
explanation of all the generators.

As expected, all tests passed, giving the following output:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

This means that our implementations of set operators are most likely correct.

-}