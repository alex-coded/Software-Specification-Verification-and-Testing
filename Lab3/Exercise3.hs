-- Names: Alexandra Volentir, Daria Protsenko, Nora Silven, Shuqi Yi.
-- UvA student IDs: 15257304, 12856991, 13223585, 12513938.
-- Study: MSc Software Engineering.
-- This program is intended to implement a function that calculates the minimal property subsets.
-- Time spent: 10 hours.

module Exercise3 where

--Imports
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace

instance Show (([Integer] -> Integer -> Bool)) where
       show a = "property"

--Implement a function that calculates the minimal property subsets, given a 'function under
--test' and a set of properties.
minPropSubset :: ([[Integer] -> Gen [Integer]]) -> (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> IO [([Integer] -> Integer -> Bool)]
minPropSubset mutator fut propList = do

    --Generate a flattened table of property results
    results <- generate (mapM (\x -> sequence [fmap and (mutate' y [x] fut 3) | y <- mutator]) propList) --[[Bool]]
    --Example: [[False,False,False],[False,True,False],[False,False,False],[False,True,False],[False,True,False]]

    --Zip the propList and results together
    let zippedList = zip propList results --list of tuples
    --traceM $ "Zipped List: " ++ show (length zippedList)
    --traceM $ "Zipped List: " ++ show zippedList

    --Filter out items where all elements in the corresponding inner list are True (property kills no mutant)
    let filteredList = filter (\(_, innerList) -> (not (all (== True) innerList))) zippedList
    --traceM $ "Filtered List: " ++ show filteredList

    --Filter out all items if two inner lists are identical -> they have the same elements in the same order
    --(property kills the same mutants as other property)
    let finalFilteredList = nubBy (\(_, lst1) (_, lst2) -> lst1 == lst2) filteredList

    --Extract the resulting list of properties from the filtered result
    let finalResults = map fst finalFilteredList

    --ALTERNATIVE CODE - same issue
    --let finalResults =  [prop | (prop, results) <- zip propList results, all (/= True) results]

    return finalResults

main :: IO [([Integer] -> Integer -> Bool)]
main = do
    results <- minPropSubset mutators multiplicationTable multiplicationTableProps
    return results


{-
--------- LIBRARIES ---------

In this program, the following libraries are used:
- Test.QuickCheck: To be able to make use of mutators, which are of type Gen.
- MultiplicationTable: The function and properties described here are used to test our countSurvivors
  function.
- Mutation: The mutate' function is used in the countSurvivors function, and some standard mutators are
  used for testing.
- Debug.Trace: do debug the code by using the function traceM.
- Data.List: to use the function nubBy to remove duplicates from the list.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to calculate the minimal property subsets, given a 'function under
test' and a set of properties. The function also takes a mutator, which is a list of mutations
for which the MPS is calculated.

The definition of a reduntant property is as follows: If a property kills no mutant, or it kills the same mutants
as another property, it can be considered redundant over other properties.

In implementing this problem, we used the following visualisation:

KILLS THE MUTANT = FALSE
DOESN'T KILL THE MUTANT = TRUE

Example:
          prop1 prop2 prop3 prop4
mutation1 True  True  True  True
mutation2 True  False False False
mutation3 True  True  False  True

This table represents the full property set and how it interacts with different mutations inside the mutator.
To find MPS, we look at the columns in this table. If the column consists of only True values, that property is redundant.
If two columns are the same, only one duplicate should be kept. The rest of the properties are redundant.

The folllowing steps are followed in the program:

1. For every property in propList, we apply function mutate' separately to determine how it interacts with that mutant.
We use mutate' and not mutate to avoid the Maybe type.
2. We generate a list of lists, where every element is that property's "track record". In other words, every element of the
list is a column in our table.
3. We zip the propList and the result of step 2 together to create tuple pairs (property, track record).
4. We filter out items where all elements in the track record are True (property kills no mutant => redundant).
5. We filter out all items if two track records are identical (property kills the same mutants as other property => redundant).
Here we use the function nubBy which is the same as nub function (filters duplicates) but with a custom equality predicate for
our zipped list.
6. We extract the resulting filtered list of properties from result of steps 4 and 5.

One thing our program does not take into account is that the mutate' function can return an empty list.
This happens when the mutator had no effect on the output. We ran out of time to implement this.

As of now, the code compiles but does not produce any output (or produces an output with custom instance Show - see below).
In trying to diagnose this problem, one thing we did is create a custom instance Show to print objects that are properties:

instance Show (([Integer] -> Integer -> Bool)) where
      show a = "property"

When this instance is implemented, the code functions generally how we'd like it to (except you cannot see the names of properties
in MPS). We expect this could be fixed by defining another custom Show (?), or by writing an identifier for the properties -
using a tuple and storing the string name of it in the first element of the tuple.

Based on the debugging, we believe that the problem might lie in step 3 of the implementation. Perhaps zipping the two lists requires
 too much computational power and causes stack overflow.
-}
