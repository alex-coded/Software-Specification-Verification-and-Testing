import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

rot13 :: [Char] -> [Char]
rot13 [x] = [chr ((ord x) + 13)]
rot13 (x:y) = [chr ((ord x) + 13)] ++ (rot13 y)