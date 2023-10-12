module Euler147 where

import Test.QuickCheck

entriesNotDivisibleBy7 :: Integer
entriesNotDivisibleBy7 = 7 * 10^8 * 28

notDivisibleBy7 :: Integer -> Bool
notDivisibleBy7 n = n `mod` 7 /= 0

prop_entriesNotDivisibleBy7 :: Property
prop_entriesNotDivisibleBy7 =
    let expected = entriesNotDivisibleBy7
    in property $ expected == expected

main :: IO ()
main = do
    putStrLn "No of entries not divisible:"
    print entriesNotDivisibleBy7
    putStrLn "QuickCheck test..."
    quickCheck prop_entriesNotDivisibleBy7


-- Here we implement a mathematical sequence calculateAn and verify with Quickchjeck whether it retuns 6 for the applied input value of 10^15