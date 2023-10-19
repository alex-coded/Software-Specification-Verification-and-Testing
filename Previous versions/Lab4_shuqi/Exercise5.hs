module Exercise5 where
import Data.List
import Test.QuickCheck
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | (r @@ r) \\ r == [] = r
         | otherwise = trClos (union r (r @@ r))
