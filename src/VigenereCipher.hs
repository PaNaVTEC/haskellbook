module VigenereCipher where

import           Data.Char
import           Data.List
import           Data.Maybe

keyword :: String
keyword = "ALLY"

isNotSymbol :: Char -> Bool
isNotSymbol c = isLower c || isUpper c

addShift :: Int -> Char -> [Char] -> Char
addShift shift x range = cycle range !! (fromJust (elemIndex x range) + shift)

convertLetter :: Int -> Char -> Char
convertLetter 0 c     = c
convertLetter shift c = addShift shift c (rangeFor c)

calculateMask :: String -> String -> String
calculateMask keyword toEncode = snd $ foldl maskFor (0, "") toEncode
  where maskFor (i, enc) a | isNotSymbol a = (i + 1, enc ++ [(cycle keyword !! i)])
        maskFor (i, enc) a = (i, enc ++ [a])

-- 'meet at dawn' becomes 'mppr ae oywy' with they keyword 'ally'
cipher :: String -> String
cipher toEncode = zipWith (\a b -> convertLetter (shiftOf a) b) mask toEncode
  where mask = calculateMask keyword toEncode

shiftOf :: Char -> Int
shiftOf a | isNotSymbol a = maybe 0 id (elemIndex a (rangeFor a))
shiftOf _ = 0

rangeFor :: Char -> [Char]
rangeFor a | isLower a = ['a'..'z']
rangeFor a | isUpper a = ['A'..'Z']
