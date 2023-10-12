import SetOrd

type Rel a = Set (a, a)

-- Symmetric closure 
symClos :: Ord a => Rel a -> Rel a
symClos (Set pairs) = Set (pairs ++ [(y, x) | (x, y) <- pairs])

main :: IO ()
main = do
    let relation = list2set [(1, 2), (2, 3), (3, 4)]
    let symClosure = symClos relation
    print symClosure


