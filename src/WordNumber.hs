module WordNumber where

import Data.List (intersperse, intercalate)
import Data.Char

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord n = "wutttt: " ++ show n ++ " ?"

digits :: Int -> [Int]
digits n = map digitToInt $ show n

wordNumber :: Int -> String
--wordNumber n = removeLastSeparator $ concat $ map digitToSeparatedWord $ digits n
--  where separate a = a ++ "-"
--        digitToSeparatedWord = separate . digitToWord
--        removeLastSeparator = init
--wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
--wordNumber n = intercalate "-" $ map digitToWord $ digits n
wordNumber = intercalate "-" . map digitToWord . digits
