import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

squares :: Int -> Int
squares 1 = 1
squares n = n^2 + (squares $! (n - 1))

eq1 :: Int-> Int
eq1 n = n * (n+1) * (div (2*n+1) 6)

cubics :: Int -> Int
cubics 1 = 1
cubics n = n^3 + (cubics(n - 1))

eq2 :: Int -> Int
eq2 n = (n * (div (n+1) 2))^2

prop_eq1 n = squares(n) == eq1(n)
prop_eq2 n = cubics(n) == eq2(n)
