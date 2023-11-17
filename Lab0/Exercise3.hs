-- Study: MSc Software Engineering.
-- This program is intended to implement and test a function to classify a triple
-- of lengths as a type of triangle.
-- Time spend: 2 hours

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- This data shape is given in the assignment.
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

-- This function takes three integers as the sides of a triangle,
-- and determines whether the values of these sides satisfies the Pythagoras theorem.
-- This function returns True if the theorem is satisfied and False otherwise.
pyth :: Integer -> Integer -> Integer -> Bool
pyth a b c | (a^2 + b^2) == c^2 = True
           | (b^2 + c^2) == a^2 = True
           | (a^2 + c^2) == b^2 = True
           | otherwise = False

-- This function takes three integers as the sides of a triangle,
-- and determines whether this triangle is isosceles.
-- This function returns True if the triangle is isasceles and False otherwise.
checkIsosceles :: Integer -> Integer -> Integer -> Bool
checkIsosceles a b c | (a == b) && (c < 2*a) = True
                | (b == c) && (a < 2*b) =  True
                | (a == c) && (b < 2*a) =  True
                | otherwise = False

-- This function takes a triple of integers and determine if they make up the three lenghts
-- of a triangle, and of what kind.
-- Firstly, if the triplet has a negative number, it is not a triangle
-- Then, if all three sides are equal, the triangle is equilateral
-- Then, if any two sides are equal, and the third side is less than the sum of the two equal lengths,
-- the triangle is an isosceles triangle.
-- Then, when the three sides adhere to the Pythogorian theorem, the triangle is rectangular.
-- Lastly, we check the most basic property of triangle sides, since all special cases will
-- be caught by the three guards above. For three sides to be able to form a triangle, any one
-- side should be smaller than the sum of the other two sides. All triangles will be caught
-- by the middle four guards, so if a triple of lengths does not pass any of those checks, it
-- is not a triangle.
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | any (<= 0) [a, b, c] = NoTriangle
               | (a == b) && (b == c) = Equilateral
               | checkIsosceles a b c = Isosceles
               | pyth a b c = Rectangular
               | ((a + b) > c) && ((a + c) > b) && ((b + c) > a) = Other
               | otherwise = NoTriangle

-- How we checked the triangle function.
-- Due to time constraints, we only performed manual unit tests to check the triangle
-- function. We checked one testcase for each outcome option, except 2 testcases for NoTriangle.
-- For each outcome, the following lengths were used to test:
-- NoTriangle: (-1) 2 3 and 4 9 100 (Negative number and impossible combination of lengths)
-- Rectangular: 3 4 5 (3^2 + 4^2 = 5^2)
-- Equilateral: 4 4 4 (Three equal lengths)
-- Isosceles: 8 8 10 (Two equal lengths and a different third length)
-- Other: 5 6 8 (Combination of lengths that does not fit the properties of a
-- special triangle, but is a triangle nonetheless)
-- All these tests pass. However, this does not prove anything since this way of testing
-- does not cover a lot of base at all. A better way of testing would be to make generators
-- for every type of triangle (including NoTriangle) and perform quickchecks of the triangle function using those
-- generators. This would still not be solid proof, but the outcome of such tests would be
-- much more meaningful.
main :: IO ()
main = do
    quickCheck (triangle (-1) 2 3 == NoTriangle)
    quickCheck (triangle 3 4 5 == Rectangular)
    quickCheck (triangle 4 4 4 == Equilateral)
    quickCheck (triangle 8 8 10 == Isosceles)
    quickCheck (triangle 5 6 8 == Other)
    quickCheck (triangle 4 9 100 == NoTriangle)