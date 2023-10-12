module Exercise3 where

import SetOrd
import Data.List

type Rel a = [(a,a)]

-- Symmetric closure
symClos :: Ord a => Rel a -> Rel a
symClos r = nub (r ++ [(y, x) | (x, y) <- r])

main :: IO ()
main = do
    print "ex3"
