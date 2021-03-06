module Cipher where

import           Data.Char
import           Data.List
import           Data.Maybe

shift :: Int
shift = 3

addShift ::  Char -> [Char] -> Char
addShift x range = cycle range !! (fromJust(elemIndex x range) + shift)

convertLetter :: Char -> Char
convertLetter x | isLower x = addShift x ['a'..'z']
convertLetter x | isUpper x = addShift x ['A'..'Z']
convertletter otherwise = otherwise

caesarCipher :: String -> String
caesarCipher = map convertLetter

main :: IO ()
main = do
  putStr "Text to encode: "
  line <- getLine
  _ <- putStrLn $ caesarCipher line
  return ()
