-- Study: MSc Software Engineering.
-- This program is intended to implement and test the given function for generating lists of floating numbers.
-- Time spent: 2 hours


import System.Random


probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)


error_limit = 0.1
n = 100000
q = (0.0, 0.25) : (0.25, 0.50) : (0.50, 0.75) : (0.75, 1.0) : []
getBucketsCount xs = map (\(inf_limit, upper_limit) -> length $ filter (\x -> x >= inf_limit && x < upper_limit) xs) q
testBucketProportions buckets = all (\count -> abs (fromIntegral count - norm) <= stdDev) buckets
  where
    norm = fromIntegral n / fromIntegral (length q)
    stdDev = norm * error_limit


main :: IO ()
main = do
  randList <- probs n
  let buckets = getBucketsCount randList
  putStrLn $ "value of n: " ++ show n
  putStrLn $ "max std dev: " ++ show error_limit
  putStrLn $ "count in q: " ++ show buckets
  let proportionsAreCorrect = testBucketProportions buckets
  putStrLn $ "buckets in each quartile: " ++ show buckets
  putStrLn $ "proportions are correct: " ++ show proportionsAreCorrect


{-

--------- LIBRARIES ---------

We used the System. Random library for generating random numbers.

--------- Code description (purpose, features, architecture) ---------

The approach was to apply a filter for each quartile, unwrap the monad, 
Calculate how many elements we have in each bucket (where each of the buckets 
stores elements in the intervals (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1), 
and then perform the tolerance test on the buckets to see if the hypothesis of Red Curry holds.


--------- Testing approach and conclusions ---------

For testing purposes, we randomized the value of n and the maximum value of standard deviation (error limit).

  Some generic test cases, that would further are:

value of n: 4
max std dev: 0.1
count in q: [0,0,2,2]
buckets in each quartile: [0,0,2,2]
proportions are correct: False


value of n: 4
max std dev: 0.5
count in q: [1,1,1,1]
buckets in each quartile: [1,1,1,1]
proportions are correct: True


value of n: 4
max std dev: 0.5
count in q: [1,0,0,3]
buckets in each quartile: [1,0,0,3]
proportions are correct: False


value of n: 50
max std dev: 0.2
count in q: [14,12,13,11]
buckets in each quartile: [14,12,13,11]
proportions are correct: True


value of n: 100
max std dev: 0.1
count in q: [24,27,24,25]
buckets in each quartile: [24,27,24,25]
proportions are correct: True


value of n: 200
max std dev: 0.1
count in q: [44,49,46,61]
buckets in each quartile: [44,49,46,61]
proportions are correct: False


value of n: 200
max std dev: 0.1
count in q: [52,49,50,49]
buckets in each quartile: [52,49,50,49]
proportions are correct: True


value of n: 1000
max std dev: 0.1
count in q: [239,232,264,265]
buckets in each quartile: [239,232,264,265]
proportions are correct: True

value of n: 100000
max std dev: 0.1
count in q: [25184,25107,24929,24780]
buckets in each quartile: [25184,25107,24929,24780]
proportions are correct: True



If I calculate for less than 25 values, with a big deviation of around 50%, the property holds.
For values between 4 and 200, if we had a big deviation, the property would practically always hold.
Therefore, we concluded that the bigger the number, the smaller the deviation, and vice versa,
the less the number, the bigger the deviation.


On the same note, we found out that the randomization is TRUE, because for the majority of 
the buckets with the value of generated numbers larger than values of 4..100 and with a relatively small
standard deviation, we get a consistent number of elements in each of the buckets.

To trace final logical reasoning, we could state that for small values that don't exceed 100, the
the probability that this property will hold is 2/3 True for the most of test cases.

For values larger than 200, the probability of finding that the probability doesn't hold is around 1/10.

While for values larger than 10000 the property always held for 200 manual tests.

As per se, after the randomization of the maximum standard deviation and the value of n, we
found out that the property holds Ture for larger values, whereas for smaller ones - it sometimes doesn't,
because the randomness doesn't have enough space to generate a lot of numbers uniformly.

-}