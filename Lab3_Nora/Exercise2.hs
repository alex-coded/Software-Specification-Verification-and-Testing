module Exercise2 where
import Data.List
import Test.QuickCheck
import MultiplicationTable
import Mutation

-- This function counts the number of surviving mutants by using vectorOf.
countSurvivors :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countSurvivors n prop fun mutator = do
            results <- generate (vectorOf n (mutate' mutator prop fun 3))
            passed <- return (filter (\ x -> all (\y -> y == True) x) results)
            return (length passed)

-- This function counts the number of surviving mutants recursively.
countSurvivors' :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countSurvivors' n prop fun mutator = do
            results <- generate (mutate' mutator prop fun 3)
            passed <- return (all (\ x -> x == True) results)
            if n /= 0
            then do
                res <- countSurvivors (n - 1) prop fun mutator
                if passed == True
                then return (1+res)
                else return res
            else
                if passed == True
                then return 1
                else return 0

main :: IO Int
main = do
    countSurvivors 100 multiplicationTableProps multiplicationTable addElements

