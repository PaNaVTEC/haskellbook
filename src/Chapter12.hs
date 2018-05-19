module Chapter12 where

import           Data.List

-- notThe "the" -> Nothing
-- notThe "woot" -> Just 'woot'
-- notThe "blahtheblah" -> Just 'blahtheblah'
notThe :: String -> Maybe String
notThe text | text == "the" = Nothing
notThe text | text == "The" = Nothing
notThe text = Just text

-- "The cow loves us" -> "a cow loves us"
replaceThe :: String -> String
replaceThe text = unwords $ map replace (words text)
  where
    replace :: String -> String
    replace word = maybe "a" id (notThe word)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = (go 0) $ words text
  where
    go :: Integer -> [String] -> Integer
    go acc []                = acc
    go acc ("the" : word : rest) = (toInt . startsWithVowel $ word) + (go acc rest) + acc
    go acc (_ : rest)            = acc + go acc rest
    startsWithVowel []     = False
    startsWithVowel (l: _) = isVowel l

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel l = elem l vowels

toInt :: Bool -> Integer
toInt i = if i == True then 1 else 0

countPredicate :: String -> (Char -> Bool) -> Int
countPredicate sentence f = sum . fmap inWord $ words sentence
  where
    inWord :: String -> Int
    inWord word = length . filter f $ word

countVowels :: String -> Int
countVowels sentence = countPredicate sentence isVowel

countConsonants :: String -> Int
countConsonants sentence = countPredicate sentence isConsonant

newtype Word' = Word' String deriving (Eq, Show)

isConsonant :: Char -> Bool
isConsonant l = notElem l vowels && elem l ['a'..'z'] || elem l ['a'..'Z']

mkWord :: String -> Maybe Word'
mkWord t = case (countVowels t > countConsonants t) of
  True  -> Nothing
  False -> Just $ Word' t

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Num a => Nat -> a
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

natToInteger' :: Num a => Nat -> a
natToInteger' nat = sum $ unfoldr toOne nat
  where
    toOne Zero     = Nothing
    toOne (Succ n) = Just (1, n)

integerToNat :: Int -> Maybe Nat
integerToNat i | i >= 0 = Just $ foldl (\b _ -> Succ b) Zero (replicate i 1)
integerToNat _ = Nothing
