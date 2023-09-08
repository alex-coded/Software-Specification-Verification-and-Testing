import Data.List
import Data.Char
import System.Random
import Control.Monad


import Test.QuickCheck


probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)


n = 1000000
q = (0.0, 0.25) : (0.25, 0.50) : (0.50, 0.75) : (0.75, 1.0) : []
getBucketsCount xs = map (\(inf_limit, upper_limit) -> length $ filter (\x -> x >= inf_limit && x < upper_limit) xs) q
testBucketProportions buckets = all (\count -> abs (fromIntegral count - norm) <= stdDev) buckets
  where
    norm = fromIntegral n / fromIntegral (length q)
    stdDev = norm * 0.1


main :: IO ()
main = do
  randList <- probs n
  let buckets = getBucketsCount randList
  putStrLn $ "Count in q " ++ show buckets
  let proportionsAreCorrect = testBucketProportions buckets
  putStrLn $ "buckets in each quartile: " ++ show buckets
  putStrLn $ "Proportions are correct: " ++ show proportionsAreCorrect


