-- Study: MSc Software Engineering.
-- This program is intended to implement and test ROT13 encoding.
-- Time spend: 2 hours

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- This function applies the ROT13 encoding on a string.
rot13 :: [Char] -> [Char]
-- The edge case of a empty string is described, the return result would then also be a empty string.
rot13 [] = []
-- If the input string is not empty, this function starts to encrypt the first character of the input string
-- and recursively call this function on the rest of the characters in the input string till the edge case is reached.
-- There are three cases described for each character:
-- 1. the character is in the lowercase alfabet, then, the Ascii value of 'a' is subtracted from
--    the Ascii value of this character to allow modulo operation. By adding 13 and applying the
--    modulo 26 afterwards, we make sure that the 13th character after this character remains in the alfabet.
--    Then, the Acii value of 'a' willbe added back to retrieve the Ascii value van the converted character.
--    Finnaly, this result Ascii value will be converted to a character value and this function continues
--    to convert the next character.
-- 2. the character is in the uppercase alfabet, then, similar operation will be operated,
--    only the Ascii value of 'A' is used in this case.
-- 3. the character is not in the alfabet, then, the ROT13 encoding does not operate on this character.
--    Leave this character with its original value and continue to convert the next character.
rot13 (x:y) | elem x ['a'..'z'] = [chr ((rem ((ord x) - (ord 'a') + 13) 26) + ord 'a')] ++ (rot13 y)
            | elem x ['A'..'Z'] = [chr ((rem ((ord x) - (ord 'A') + 13) 26) + ord 'A')] ++ (rot13 y)
            | otherwise = [x] ++ (rot13 y)

-- ROT13 encoding replaces a letter with the 13th letter after it in the alfabet.
-- Because there are 26 letters (2×13) in the basic Latin alphabet, ROT13 is its own decryption.
-- Therefore, to test the correctness of our rot13 function, we decrypt encrypted random strings
-- and check whether the results are the same as their input.
prob_1 x = (rot13 (rot13 x)) == x

main :: IO ()
main = do
    quickCheck prob_1
