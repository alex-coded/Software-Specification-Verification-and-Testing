-- This program is intended to implement and test two equations.
-- Time spend: 2 hours

-- Imports Exercise 6.
module Exercise6 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

cnf :: Form -> Form


main :: IO ()
main = do
    quickCheck prop_eq1
    quickCheck $ forAll posGen $ prop_eq2
    quickCheck $ forAll posGen $ prop_eq3
    quickCheck $ forAll posGen $ prop_eq4

-- To prove that these statements are true, we would use induction.
