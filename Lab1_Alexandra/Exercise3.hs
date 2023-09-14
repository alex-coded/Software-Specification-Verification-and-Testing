import Data.List
import Data.Char
-- import System.Random



stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

