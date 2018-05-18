module Chapter12 where

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
    startsWithVowel (l: _) = l == 'a' || l == 'e' || l == 'i' || l == 'o' || l == 'u'
    toInt i = if i == True then 1 else 0
