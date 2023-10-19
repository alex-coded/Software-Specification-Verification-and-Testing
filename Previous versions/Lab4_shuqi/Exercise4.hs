-- Study: MSc Software Engineering.
-- This program is intended to check whether a relation is serial
-- Time spent: x hours

module Exercise4 where

import SetOrd ( inSet, list2set, Set(..) )

type Rel a = Set (a, a)

set2list :: Set a -> [a]
set2list (Set xs) = xs

-- Checking whether a relation is serial or not
isSerial :: (Eq a, Ord a) => [a] -> Rel a -> Bool
isSerial dom rel =
    let relationList = set2list rel
    in all (\x -> any (\y -> inSet (x, y) rel) dom) dom

main :: IO ()
main = do
    let dom = [1, 2, 3]
    let rel = list2set [(1, 2), (2, 3), (3, 1)]
    let flagRelationIsSerial = isSerial dom rel
    print flagRelationIsSerial
