import SetOrd

type Rel a = Set (a, a)

insertList :: (Ord a) => a -> [a] -> [a]
insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of
    GT -> y : insertList x ys'
    EQ -> ys
    _  -> x : ys

-- Add reverse of each pair to R
addRev :: Ord a => Rel a -> Rel a
addRev (Set pairs) = Set (pairs ++ [(y, x) | (x, y) <- pairs])

-- Remove duplicates
getRidOfDups :: Ord a => Rel a -> Rel a
getRidOfDups (Set pairs) = Set (foldr insertList [] pairs)

-- Symmetric closure 
symClos :: Ord a => Rel a -> Rel a
symClos rel = getRidOfDups (addRev rel)

main :: IO ()
main = do
    let relation = list2set [(1, 2), (2, 3), (3, 4)]
    let symClosure = symClos relation
    print symClosure


