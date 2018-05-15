module VigenereCipher where

import           Data.Char
import           Data.List
import           Data.Maybe

keyword :: String
keyword = "ALLY"

isNotSymbol :: Char -> Bool
isNotSymbol c = isLower c || isUpper c

addShift :: Int -> Char -> [Char] -> Char
addShift shift x range = cycle range !! (fromJust(elemIndex x range) + shift)

convertLetter :: Int -> Char -> Char
convertLetter 0 c     = c
convertLetter shift c | isLower c = cycle ['a'..'z'] !! shift
convertLetter shift c | isUpper c = cycle ['A'..'Z'] !! shift

calculateMask :: String -> String -> String
calculateMask keyword toEncode = snd $ foldl maskFor (0, "") toEncode
  where maskFor (index, enc) a | isNotSymbol a = (index + 1, enc ++ [(cycle keyword !! index)])
        maskFor (index, enc) a = (index, enc ++ [a])

cipher :: String -> String
cipher toEncode = zipWith (\a b -> convertLetter (shiftOf a) b) mask toEncode
  where mask = calculateMask keyword toEncode

shiftOf :: Char -> Int
shiftOf a | isLower a = maybe 0 id (elemIndex a ['a'..'z'])
shiftOf a | isUpper a = maybe 0 id (elemIndex a ['A'..'Z'])
shiftOf _ = 0
