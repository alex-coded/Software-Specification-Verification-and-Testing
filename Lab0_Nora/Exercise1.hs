import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- To calculate the left side of the first equation, we use the map function on a
-- list from 1 to n to square all numbers. Then we use sum to add all elements together
squares :: Int -> Int
squares 0 = 0
squares n = sum (map (\ x -> x^2) [1..n])

-- This function calculates the right side of the first equation
eq1 :: Int-> Int
eq1 n = div (n * (n+1) * (2*n+1)) 6

-- To calculate the left side of the second equation, we use the map function on a
-- list from 1 to n to perform the power operation on all numbers. Then we sum all elements.
cubics :: Int -> Int
cubics 0 = 0
cubics n = sum (map (\ x -> x^3) [1..n])

-- This function calculates the right side of the second equation.
eq2 :: Int -> Int
eq2 n = (div (n * (n+1)) 2)^2

-- This generator is used to generate numbers greater than or equal to 0, in order
-- to test the equations accurately.
posGen :: Gen Int
posGen = suchThat (arbitrary :: Gen Int) (>= 0)

-- For both equations we exclude negative numbers. Then we check if both sides of
-- the equation are equal.
prop_eq1 n = squares(n) == eq1(n)
prop_eq2 n = cubics(n) == eq2(n)

main :: IO ()
main = do
    quickCheck $ forAll posGen $ prop_eq1
    quickCheck $ forAll posGen $ prop_eq2

-- To prove that these statements are true, we would use induction.
