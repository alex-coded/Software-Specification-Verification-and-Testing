import Test.QuickCheck

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | even n || n `mod` 3 == 0 = False
  | otherwise = go 5
  where
    go i
      | i * i > n = True
      | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
      | otherwise = go (i + 6)

reverseNumber :: Integer -> Integer
reverseNumber n = read (reverse (show n))

isReversiblePrimeSquare :: Integer -> Bool
isReversiblePrimeSquare n =
  not (isPalindrome n) &&
  isPrime (isqrt n) &&
  isPrime (isqrt (reverseNumber n))
  where
    isqrt = floor . sqrt . fromIntegral
    isPalindrome n' = show n' == reverse (show n')

reversiblePrimeSquares :: [Integer]
reversiblePrimeSquares = take 50 [x^2 | x <- [1..], isReversiblePrimeSquare (x^2)]


main :: IO ()
main = do
  let sumOfSquares = sum reversiblePrimeSquares
  putStrLn $ "The sum of the first 50 reversible prime squares is: " ++ show sumOfSquares
