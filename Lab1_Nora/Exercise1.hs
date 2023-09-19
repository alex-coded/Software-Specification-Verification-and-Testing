module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n - 1))

posGen :: Gen Integer
posGen = suchThat (arbitrary :: Gen Integer) (> 0)

posGen2 :: Gen Integer
posGen2 = suchThat (arbitrary :: Gen Integer) (> 1)

prop_div n = div (factorial n) n == factorial (n - 1)
prop_pos n = factorial n > 0
prop_even n = mod (factorial n) 2 == 0

main :: IO ()
main = do
    quickCheck (forAll posGen prop_div)
    quickCheck (forAll posGen prop_pos)
    quickCheck (forAll posGen2 prop_even)


{-
--------- LIBRARIES ---------



--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------



--------- TESTING APPROACH ---------



-}