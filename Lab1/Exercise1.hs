-- This program is intended to implement and test two equations.
-- Time spend: 2 hours

module Exercise1 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- The factorial of the edge case 0 is 1.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n - 1))

-- To prove that these statements are true, we use induction.
prop_eq1 :: Integer -> Property
prop_eq1 n = (n > 0) ==> ((div (factorial n) n) == (factorial (n - 1)))

prop_eq2 :: Integer -> Property
prop_eq2 n = (n > 0) ==> ((factorial (n - 1)) * n == (factorial n))

prop_eq3 :: Integer -> Property
prop_eq3 n = (n > 1) ==> ((rem (factorial n) 2) == 0)

main :: IO ()
main = do
    quickCheck prop_eq1
    quickCheck prop_eq2
    quickCheck prop_eq3
