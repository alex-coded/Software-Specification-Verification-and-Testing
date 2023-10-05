module Exercise2 where
import Data.List
import Test.QuickCheck
import MultiplicationTable
import Mutation

countSurvivors :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Int
countSurvivors n prop fun = do
            results <- generate (mutate' addElements prop fun 3)
            passed <- return (all (\ x -> x == True) results)
            if n /= 0
            then do
                res <- countSurvivors (n - 1) prop fun
                if passed == True
                then return (1+res)
                else return (res)
            else
                if passed == True
                then return (1)
                else return (0)

main :: IO Int
main = do
    countSurvivors 100 multiplicationTableProps multiplicationTable

-- for ( number of mutants )
--     output = multiplicationTable ( input )
--     mutant = mutator ( output )
--     result = property ( mutant ) ( n )
--     if ( result ) then survivors += 1

-- maybe repeat mutate function 4000 (amount of mutants) times?
