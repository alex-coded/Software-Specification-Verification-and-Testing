-- Study: MSc Software Engineering.
-- This program is intended to showcase two generators that generate data type Set Int
-- Time spent: 2 hours

module Exercise1 where

import System.Random
import Control.Monad
import SetOrd
import Test.QuickCheck


getRandomSet :: Int -> Int -> Int -> IO (Set Int)
getRandomSet min max n = do
    list <- replicateM n $ randomRIO (min, max)
    return $ list2set list

genQuickCheck :: Gen (Set Int)
genQuickCheck = do
    list2set <$> arbitrary

main :: IO ()
main = do
    set <- getRandomSet 100 400 25
    print set
    
{-

--------- LIBRARIES ---------
I used the Control.Monad library because of the replicateM function, which was needed
in getRandom set. Basically, Control.Monad helps us generate a list of monadic values. 
Here, we use randomRIO to generate n values in a given interval.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

1. First generator
The first generator - getRandom set is a function that takes 3 integer parameters, in particular:
* minimum (lower range value)
* maximum (uper range value)
* n - number of parameters to be generated within the function

Within this function, we use randomRIO to generate the random numbers in the interval [min, max].

ReplicateM helps us create monadic values with randomRIO.

2. Second generator

The second generator produces the desired type Set Int with using the arbitraty function
given by QuickCheck to generate random values for the passed datatype

<$> maps the function list2set over the generated lists of arbitrary 
-}


