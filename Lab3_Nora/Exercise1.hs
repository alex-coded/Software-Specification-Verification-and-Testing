module Exercise1 where
import Data.List
import Test.QuickCheck
import Mutation

-- POSSIBLE MUTATORS --

-- Shuffle list
--

shuffleList :: [Integer] -> Gen [Integer]
shuffleList xs = shuffle xs