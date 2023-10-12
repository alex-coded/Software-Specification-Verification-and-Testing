import SetOrd
import Data.List

type Rel a = Set (a, a)

-- Symmetric closure
96618c646c2ead409b01
symClos :: Ord a => Rel a -> Rel a
symClos (Set pairs) = Set (nub(pairs ++ [(y, x) | (x, y) <- pairs]))

main :: IO ()
main = do
    let relation = list2set [(1, 2), (2, 1), (3, 4)]
    let symClosure = symClos relation
    print symClosure


