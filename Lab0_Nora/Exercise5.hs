import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 [x] | elem x ['a'..'z'] = [chr ((rem ((ord x) - (ord 'a') + 13) 26) + ord 'a')]
          | elem x ['A'..'Z'] = [chr ((rem ((ord x) - (ord 'A') + 13) 26) + ord 'A')]
          | otherwise = [x]
rot13 (x:y) = (rot13 [x]) ++ (rot13 y)

prob_1 x = (rot13 (rot13 x)) == x

main :: IO ()
main = do
    quickCheck prob_1