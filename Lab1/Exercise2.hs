-- Names: Alexandra Volentir, Dasha Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to implement and test two equations.
-- Time spend: 2 hours

module Exercise2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

my_subsequences :: [a] -> [[a]]
my_subsequences [] = [[]]
my_subsequences (h:t) = [h:ps | ps <- my_subsequences t] ++ my_subsequences t

posGen :: Gen Integer
posGen = suchThat (arbitrary :: Gen Integer) (>= 1)

prop_eq1 = length (my_subsequences []) == 1

prop_eq2 :: Integer -> Bool
prop_eq2 n = length (my_subsequences [1..n]) == 2^n


prop_eq3 :: Integer -> Bool
prop_eq3 n = length(my_subsequences [1..n]) * 2 == length(my_subsequences [1..(n + 1)])

prop_eq4 :: Integer -> Bool
prop_eq4 n = (my_subsequences [1..n]) == (subsequences [1..n])

main :: IO ()
main = do
    quickCheck prop_eq1
    quickCheck $ forAll posGen $ prop_eq2
    quickCheck $ forAll posGen $ prop_eq3
    quickCheck $ forAll posGen $ prop_eq4

-- To prove that these statements are true, we would use induction.
