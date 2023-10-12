
module Euler551 where

import Test.QuickCheck

calculateAn :: Integer -> Integer
calculateAn n
    | n `mod` 6 == 0 = 7 
    | otherwise = [1, 1, 2, 4, 6, 7] !! fromIntegral (n `mod` 6)

prop_calculateAn_10_15 :: Bool
prop_calculateAn_10_15 = calculateAn (10^15) == 6

main :: IO ()
main = do
    putStrLn "Testing..."
    quickCheck prop_calculateAn_10_15