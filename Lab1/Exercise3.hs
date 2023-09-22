-- Study: MSc Software Engineering.
-- This program is intended to define properties, compare their strengths, and sort these by strength in descending order. 
-- Time spend: 3 hours


module Exercise2 where

import Lecture2

prop1 :: Int -> Bool
prop1 x = even x && x >= 4

prop2 :: Int -> Bool
prop2 x = even x || x >= 4

prop3 :: Int -> Bool
prop3 x = (even x && x >= 4) || even x

prop4 :: Int -> Bool
prop4 x = even x || x < 4

listOfProperties :: [(String, Int -> Bool)]
listOfProperties =
  [ ("prop1", prop1),
    ("prop2", prop2),
    ("prop3", prop3),
    ("prop4", prop4)
  ]

propCpmparisson :: [(String, Int -> Bool)] -> [(String, [String])]
propCpmparisson props =
  [(pName, [oName | (oName, _) <- props, pName /= oName && stronger [-10..10] (propertyByName pName) (propertyByName oName)]) | (pName, _) <- props]
  where
    propertyByName pName x = case lookup pName listOfProperties of
      Just prop -> prop x
      Nothing -> False

getPropertiesByDescStrength :: [(String, [String])] -> [(String, [String])]
getPropertiesByDescStrength = quicksort


main :: IO ()
main = do
  let comparisons = propCpmparisson listOfProperties
  let sortedProps = getPropertiesByDescStrength comparisons
  mapM_ (\(propName, _) -> putStrLn propName) sortedProps



  --------- LIBRARIES ---------

  -- We didn't use any additional libraries for this exercise.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

-- Implementation of properties --

-- We have four properties defined: prop1, prop2, prop3, prop4. These 
-- properties, as in the description, take an integer as input and return 
-- a bool result after analyzing the conditions. 

-- With the listOfProperties we define a list of tuples (where each tuple is made of 2 elements:
-- the property name and the corresponding prop function). In short, we combine the name of the property
-- with its corresponding function for later printing.

-- One of the most essential functions is the propComparisson, which compares the strengths of the properties
-- (given the initial function `stronger` that can compare 2 properties, for example, p and q on a set of values xs. 
-- What the stronger function does is check whether one property is more restrictive than another,
-- iterating on all the xs values with the function forall).
-- [oName | (oName, _) <- props, pName /= oName && stronger [-10..10] (propertyByName pName) (propertyByName oName)] with
-- this code we generate a list of prop names weaker than pName on the domain [-10, 10], udolizing of course the stronger function.

-- Later we sort the properties with the getPropertiesByDescStrength function, which takes a list of name-list `weaker` prop
-- names pairs ==> sorting by strength in descending order using the quicksort function given in Lecture2. 


-- Descending the strength list of said implementation (figuring out how to print them and providing my answer)--

-- Printing the properties--

-- I figured that in order to print the properties they need to be combined with their name.
-- To print the properties in the end, after figuring out their order, I used the high-order function
-- mapM_, that takes 2 arguments: the function that will be applied to each element of the list, and the list of elements to be printed.
-- \ (propName, _) -> putStrLn propName is an anonymous lambda function that takes (propName, _) as an input, and,
-- within the lambda function putStrLn is used to print the propName to console.


--------- TESTING APPROACH ---------

-- The condition didn't ask for any testing to validate the code. We could, indeed, verify that the properties are correct
-- by giving a larger domain, and, more, validating whether the sorting produces the desired results and properties behave as expected.
-- I could've also tested this code for another set of properties to test whether the comparissons and sorting produce the desired results. 
