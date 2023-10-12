-- Study: MSc Software Engineering.
-- Lab 4 - Week 6. Invariant and Relations
-- Time spent: x minutes

module Exercise6 where

import Data.List
import Test.QuickCheck
import SetOrd

type Rel a = [(a,a)]

-- Exercise 3: Symmetric closure
symClos :: Ord a => Rel a -> Rel a
symClos r = nub (r ++ [(y, x) | (x, y) <- r])

-- Exercise 5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | (r @@ r) \\ r == [] = r
         | otherwise = trClos (union r (r @@ r))

-- Checking whether a relation is symmetric or not

-- Symmetricity: for every tuple (x,y) look if there is (y,x)
isSymmetric :: (Eq a, Ord a) => Rel a -> Bool
isSymmetric r = let symmetric = symClos r
                in and [elem (y, x) symmetric | (x, y) <- symmetric]

-- Transtivity: for every tuple (x,y) where (y,z) is true, (x,z)
isTransitive :: (Eq a, Ord a) => Rel a -> Bool
isTransitive r = let transitive = trClos r
                 in and [elem (x, t) transitive| (x, y) <- transitive, (z, t) <- transitive, y == z]

main :: IO ()
main = do
    quickCheck $ forAll (arbitrary :: Gen [(Int, Int)])$ isSymmetric
    quickCheck $ forAll (arbitrary :: Gen [(Int, Int)])$ isTransitive
