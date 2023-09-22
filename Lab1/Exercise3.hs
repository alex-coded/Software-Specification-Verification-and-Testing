module Exercise2 where

import Lecture2
-- imported quicksort and stronger

prop1 :: Int -> Bool
prop1 x = even x && x >= 4

prop2 :: Int -> Bool
prop2 x = even x || x >= 4

prop3 :: Int -> Bool
prop3 x = (even x && x >= 4) || even x

prop4 :: Int -> Bool
prop4 x = even x || x < 4

properties :: [(String, Int -> Bool)]
properties =
  [ ("prop1", prop1),
    ("prop2", prop2),
    ("prop3", prop3),
    ("prop4", prop4)
  ]

-- Compare properties by strength
propCpmparisson :: [(String, Int -> Bool)] -> [(String, [String])]
propCpmparisson props =
  [(pName, [otherName | (otherName, _) <- props, pName /= otherName && stronger [-10..10] (propertyByName pName) (propertyByName otherName)]) | (pName, _) <- props]
  where
    propertyByName pName x = case lookup pName properties of
      Just prop -> prop x
      Nothing -> False

-- Sort properties by strength (using the given quicksort function)
getPropertiesByDescStrength :: [(String, [String])] -> [(String, [String])]
getPropertiesByDescStrength = quicksort


main :: IO ()
main = do
  let comparisons = propCpmparisson properties
  let sortedProps = getPropertiesByDescStrength comparisons
  mapM_ (\(propName, _) -> putStrLn propName) sortedProps
  