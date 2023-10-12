-- Study: MSc Software Engineering.
-- This program is intended to implement a random data generator for data type Set Int (from skratch and then a QiuckCheck version to random test)
-- Time spent: x hours

module Exercise1 where

import System.Random
import Test.QuickCheck

import SetOrd

-- Generate a random list of integers
genList :: Gen [Int]
genList = arbitrary

-- Define a generator for Set Int
genSet :: Gen (Set Int)
genSet = do
  list2set <$> genList

-- Define an Arbitrary instance for Set Int
instance Arbitrary (Set Int) where
  arbitrary = genSet

set2list :: Set a -> [a]
set2list (Set xs) = xs


prop_inSet :: Int -> Set Int -> Bool
prop_inSet x s = inSet x s == elem x (set2list s)

-- Property 2: Testing if the Set is empty
prop_isEmpty :: Set Int -> Bool
prop_isEmpty s = isEmpty s == null (set2list s)

-- Property 3: Testing subset relation
prop_subSet :: Set Int -> Set Int -> Bool
prop_subSet s1 s2 = subSet s1 s2 == all (\x -> inSet x s2) (set2list s1)

-- Property 4: Testing insertion
prop_insertSet :: Int -> Set Int -> Bool
prop_insertSet x s = inSet x (insertSet x s)

-- Property 5: Testing union
prop_unionSet :: Set Int -> Set Int -> Bool
prop_unionSet s1 s2 = all (\x -> inSet x s1 || inSet x s2) (set2list (unionSet s1 s2))


main :: IO ()
main = do
  quickCheck prop_inSet
  quickCheck prop_isEmpty
  quickCheck prop_subSet
  quickCheck prop_insertSet
  quickCheck prop_unionSet

