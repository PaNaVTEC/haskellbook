module VigenereCipher where

import           Data.Char
import           Data.List
import           Data.Maybe

keyword :: String
keyword = "ALLY"

isUppercase :: Char -> Bool
isUppercase c = ord c `elem` [97..122]

isLowercase :: Char -> Bool
isLowercase c = ord c `elem` [65..90]

isNotSymbol :: Char -> Bool
isNotSymbol c = isLowercase c || isUppercase c

addShift :: Int -> Char -> [Char] -> Char
addShift shift x range = cycle range !! (fromJust(elemIndex x range) + shift)

convertLetter :: Int -> Char -> Char
convertLetter shift x | isUppercase x = addShift shift x ['a'..'z']
convertLetter shift x | isLowercase x = addShift shift x ['A'..'Z']
convertletter otherwise = otherwise

cipher :: String -> String
cipher toEncode = zipWith (\a b -> convertLetter (shiftOf a) b) mask toEncode
  where mask = calculateMask keyword toEncode
        shiftOf a | isLowercase a = maybe 0 id (elemIndex a ['a'..'z'])
        shiftOf a | isUppercase a = maybe 0 id (elemIndex a ['A'..'Z'])

calculateMask :: String -> String -> String
calculateMask keyword toEncode = snd $ foldl maskFor (0, "") toEncode
  where maskFor (index, enc) a | isNotSymbol a = (index + 1, (cycle keyword !! index) : enc)
        maskFor (index, enc) a = (index, a : enc)
