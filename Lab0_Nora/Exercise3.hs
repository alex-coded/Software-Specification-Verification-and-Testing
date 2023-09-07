import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

pyth :: Integer -> Integer -> Integer -> Bool
pyth a b c | (a^2 + b^2) == c^2 = True
           | (b^2 + c^2) == a^2 = True
           | (a^2 + c^2) == b^2 = True
           | otherwise = False

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | any (<= 0) [a, b, c] = NoTriangle
               | (a == b) && (b == c) = Equilateral
               | ((a == b) && (c < 2*a)) || ((a == c) && (b < 2*a)) || ((b == c) && (a < 2*b)) = Isosceles
               | pyth a b c = Rectangular
               | ((a + b) > c) && ((a + c) > b) && ((b + c) > a) = Other
               | otherwise = NoTriangle

