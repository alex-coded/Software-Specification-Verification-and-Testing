module Exercise2 where

import System.Random
import Test.QuickCheck
-- import Lecture1

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:y) = [x:z | z <- subsequences y] ++ subsequences y

posGen :: Gen Integer
posGen = suchThat (arbitrary :: Gen Integer) (> 0)

prop n = length (subsequences [1..n]) == 2^n

main :: IO ()
main = do
    quickCheck (forAll posGen prop)


{-
--------- LIBRARIES ---------



--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------



--------- TESTING APPROACH ---------



-}