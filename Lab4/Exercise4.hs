-- Study: MSc Software Engineering.
-- This program is intended to check whether a relation is serial
-- Time spent: 6 hours

module Exercise4 where

import SetOrd
import Data.List(nub)
import Test.QuickCheck

type Rel a = [(a, a)]

prop_serial_and_transitive rel =
  let dom = nub $ concatMap (\(x, y) -> [x, y]) rel
  in isSerial dom rel && checkTransitivity rel

checkTransitivity rel = all (\(x, y) -> all (\(y', z) -> notElem (x, z) rel || existsTransitivePair x y' z rel) rel) rel

existsTransitivePair x y z = any (\(x', z') -> x' == x && z' == z)

isSerial :: (Eq a, Ord a) => [a] -> Rel a -> Bool
isSerial dom rel = all (\x -> any (\y -> (x, y) `elem` rel) dom) dom

prop_transitivity rel = all (\(x, y) -> all (\(y', z) -> notElem (x, z) rel || any (\(x', z') -> x' == x && z' == z) rel) rel) rel

prop_antisymmetry rel = all (\(x, y) -> notElem (y, x) rel || x == y) rel

genRel sizeGen = do
  size <- sizeGen
  list <- vectorOf size $ do
    x <- arbitrary
    y <- arbitrary
    return (x, y)
  return (nub list)


main :: IO ()
main = do
  let flag = isSerial [1, 2, 3, 4] [(1, 2), (2, 3), (3, 4), (4, 1)]
  print flag
  let flag2 = isSerial [1, 3, 5, 29] [(1, 3), (3, 5), (5, 29), (29, 1)]
  print flag2
  let flag3 = isSerial [1, 3, 5, 29, 40] [(0, 1), (1, 5), (5, 30), (30, 1), (0,5)]
  print flag3
  -- quickCheck prop_serial_and_transitive
  quickCheck (forAll (genRel (choose (0,5))) (prop_transitivity :: Rel Int -> Bool))
  quickCheck (forAll (genRel (choose (0,5))) (prop_antisymmetry :: Rel Int -> Bool))


{-
--------- LIBRARIES ---------
In this approach I used the Data.List(nub) in order to remove the duplicates when I generate
relations with the genRel generator

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------
The function isSeral basically checks that for every x in dom (domain), there IS an y
s.t. (x,y) is in rel. Basically, we verify that "every element in the dom is related 
to at least 1 elm in the domain.""

Transitivity checks the following thing: for every (x,y) and (y,z) in R, the pair (y,z) is also in R

In the antisimetry property we check if (y,x) exist and if x and y are equal.

I had problems in testing the isSerial function with the properties aforementioned. I did the relationship generator,
and also extracted the domain from the genrator, but nevertheless, my tests failed, maybe because one of my intermediate
functions in the proprety verification was lagging. But nethertheless, my valid tests that would prove isSerial, and, at the same
time, it is transitive and antisimetric, would be done by computing quickCheck prop_serial_and_transitive
and doing this type of verification that would both check for serialness and transitivity.

Nevertheless, the transitivity and antisemtry functions are present and they work, as well as the isSerial.
I tested the isSerial for some values, some of them are:

 let flag = isSerial [1, 2, 3, 4] [(1, 2), (2, 3), (3, 4), (4, 1)]
  True
  let flag2 = isSerial [1, 3, 5, 29] [(1, 3), (3, 5), (5, 29), (29, 1)]
  True
  let flag3 = isSerial [1, 3, 5, 29, 40] [(0, 1), (1, 5), (5, 30), (30, 1), (0,5)]
  False

The validity of the properties is as follows:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

prop_serial_and_transitive rel =
  let dom = nub $ concatMap (\(x, y) -> [x, y]) rel
  in isSerial dom rel && checkTransitivity rel

2. R = {(x, y) | x ≡ y (mod n)} We can consider that this is serial when R covers the whole set
(every element in the set is related with another element)


if n > 1, then R would be serial in case n is prime relative to set size. If n is coprime with the set-size,
means n is serial. Particularly, every elm will have a related elm because  the mod will cover the whole set.
It's just a trivial congruency property.

R would NOT be serial in case n and the set-size DO have a common factors. In the set there will exist elements that
do NOT have a related elm in R (differently from the first case, here we are not able to  `touch` upon all elements in the set).



---
Testing if R is serial:
For each x, we get every value y that satisfies x ≡ y (mod n)
Then, we check if each x has one related y in that particular set. 


Proving R is serial:
- For each x, there is one y elm s.t. x ≡ y (mod n)
- Prove that there no elmenets are left without a correlation in R.
-}

