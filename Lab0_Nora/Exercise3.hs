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

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | any (<= 0) [a, b, c] = NoTriangle
               | (a == b) && (b == c) = Equilateral
               | checkIsosceles a b c = Isosceles
               | pyth a b c = Rectangular
               | ((a + b) > c) && ((a + c) > b) && ((b + c) > a) = Other
               | otherwise = NoTriangle

