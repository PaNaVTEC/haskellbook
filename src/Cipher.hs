module Cipher where

import           Data.Char
import           Data.List
import           Data.Maybe

shift :: Int
shift = 3

isUppercase :: Char -> Bool
isUppercase c = (ord c) `elem` [97..122]

isLowercase :: Char -> Bool
isLowercase c = (ord c) `elem` [65..90]

addShift ::  Char -> [Char] -> Char
addShift x range = cycle range !! (fromJust(elemIndex x range) + shift)

convertLetter :: Char -> Char
convertLetter x | isUppercase x = addShift x ['a'..'z']
convertLetter x | isLowercase x = addShift x ['A'..'Z']
convertletter otherwise = otherwise

caesarCipher :: String -> String
caesarCipher = map (\x -> convertLetter x)
