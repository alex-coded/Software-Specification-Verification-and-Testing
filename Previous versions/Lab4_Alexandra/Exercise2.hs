module Exercise2 where
import SetOrd

import Test.QuickCheck
import Data.List
import SetOrd


setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set [x | x <- xs, contains x ys]

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = Set (xs ++ [y | y <- ys, not (inSet y xs)])

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = Set [x | x <- xs, not (inSet x ys)]

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
    in all (\x -> inSet x result) set1 && all (\x -> not (inSet x result)) set2

main :: IO ()
main = do
    quickCheck prop_intersection
    quickCheck prop_union
    quickCheck prop_difference
