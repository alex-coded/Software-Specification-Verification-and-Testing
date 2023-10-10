-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to 
-- Time spent: 

module Exercise3 where

--Imports
import Data.List
import Test.QuickCheck
import Mutation
--import FitSpec
import MultiplicationTable
import Debug.Trace
import Exercise2
import Control.Monad

--instance Show (([Integer] -> Integer -> Bool)) where
--    show a = "property"

instance Show (([Integer] -> Integer -> Bool)) 

--Implement a function that calculates the minimal property subsets, given a 'function under
--test' and a set of properties.
minPropSubset :: ([[Integer] -> Gen [Integer]]) -> (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> IO [([Integer] -> Integer -> Bool)]
minPropSubset mutator fut propList = do

    --Generate a flattened table of property results
    results <- generate (mapM (\x -> sequence [fmap and (mutate' y [x] fut 3) | y <- mutator]) propList) --[[Bool]]
    --[[False,False,False],[False,True,False],[False,False,False],[False,True,False],[False,True,False]]

    --Zip the propList and results together
    let zippedList = zip propList results --list of tuples
    --traceM $ "Zipped List: " ++ show (length zippedList)
    --traceM $ "Zipped List: " ++ show (take 1 zippedList)

    --Filter out items where all elements in the corresponding inner list are True
    let filteredList = filter (\(_, innerList) -> (not (all (== True) innerList))) zippedList
    --traceM $ "Filtered List: " ++ show (length filteredList)

    --Filter out all items if two inner lists are identical -> they have the same elements in the same order
    let finalFilteredList = nubBy (\(_, lst1) (_, lst2) -> lst1 == lst2) filteredList
    --traceM $ "Final filtered List: " ++ show (length finalFilteredList)

    --Extract the resulting list of properties from the filtered result
    let finalResults = map fst finalFilteredList
    --traceM $ "Final filtered List: " ++ show (length finalResults)

    --ALTERNATIVE CODE - same issue
    --let finalResults =  [prop | (prop, results) <- zip propList results, all (/= True) results]
    --let finalResults = [prop | i <- [0 .. length results - 1], let prop = propList !! i, let boolList = results !! i, all (/= True) boolList]
    --traceM $ "finalResults " ++ show (length finalResults)
    --traceM $ "finalResults " ++ show (take 1 finalResults)

    return finalResults

{-
KILLS THE MUTANT = FALSE
DOESN'T KILL THE MUTANT = TRUE 

          prop1 prop2 prop3 prop4
mutation1 True  True  True  True 
mutation2 True  False False False
mutation3 True  True  False  True


false false false false false
false true false  true  true 
false false  false  false  false
-}

{-
If a property kills no mutant, or it kills the same mutants as another property, it can be
considered redundant over other properties.

Take a set of properties

If countSurvivors for this property (number of mutants) = number of mutations (all survived -> killed no mutants)
= the property for which it was tested is redundant -> exclude it from set

If mutate (mutator, property1, function, input) = mutate (mutator, property2, function, input) = property2 is redundant 
-> GEORGIA NOTE -> CANNOT DO BC COMPARING DIFFERENT THINGS 

return the minimal set of properties

-}

--I ENDED UP NOT USING THIS

--REWRITE THE MUTATE FUNCTION TO INCLUDE TWO PROPERTIES
--Applies a mutator to TWO PROPERTIES and function under test, then returns TRUE IF THE PROPERTIES KILL THE SAME MUTANT
mutate'' :: Eq a => (a -> Gen a) -> (a -> Integer -> Bool) -> (a -> Integer -> Bool) -> (Integer -> a) -> Integer -> Gen Bool
mutate'' mutator prop1 prop2 fut input = mutation >>= \mutant -> propertyExecutor'' prop1 prop2 mutant input
        where output = fut input
              mutation = mutator output

--Returns TRUE IF THE PROPERTIES KILL THE SAME MUTANT 
propertyExecutor'' :: Eq a => (a -> Integer -> Bool) -> (a -> Integer -> Bool) -> a -> Integer -> Gen Bool
propertyExecutor'' prop1 prop2 mutant x = pure (prop1 mutant x == prop2 mutant x)

main :: IO [([Integer] -> Integer -> Bool)]
main = do
    --results <- generate (mutate'' addElements prop_tenElements prop_firstElementIsInput multiplicationTable 3)
    --results <- filterPropSubset addElements multiplicationTable multiplicationTableProps
    results <- minPropSubset mutators multiplicationTable multiplicationTableProps
    return results
    



