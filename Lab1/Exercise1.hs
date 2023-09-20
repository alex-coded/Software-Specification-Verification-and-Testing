-- Names: Alexandra Volentir, Dasha Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
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

posGen :: Random Integer
posGen = suchThat (arbitrary :: Gen Integer) (>= 0)

posGen2 :: Random Integer
posGen2 = suchThat (arbitrary :: Gen Integer) (>= 1)

-- To prove that these statements are true, we use induction.
prop_eq1 :: Integer -> Bool
prop_eq1 n = (div (factorial n) n) == (factorial (n - 1))

prop_eq2 :: Integer -> Bool
prop_eq2 n = (factorial (n - 1)) * n == (factorial n)

prop_eq3 :: Integer -> Bool
prop_eq3 n | (n <= 1) = factorial n == 1
           | (n > 1) = (rem (factorial n) 2) == 0

main :: IO ()
main = do
    quickCheck $ forAll posGen2 $ prop_eq1
    quickCheck $ forAll posGen2 $ prop_eq2
    quickCheck $ forAll posGen $ prop_eq3

