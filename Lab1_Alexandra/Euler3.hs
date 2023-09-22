-- problem 551


calculateAn :: Integer -> Integer
calculateAn n
    | n `mod` 6 == 0 = 7 
    | otherwise = [1, 1, 2, 4, 6, 7] !! fromIntegral (n `mod` 6)

main :: IO ()
main = do
    let n = 10^15
    let result = calculateAn n
    print result