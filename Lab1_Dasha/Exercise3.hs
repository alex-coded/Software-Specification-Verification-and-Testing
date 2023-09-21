import Data.List
--import System.Random
--import Test.QuickCheck ()
import Lecture2.hs

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\\ x --> p x --> q x)
weaker   xs p q = stronger xs q p


