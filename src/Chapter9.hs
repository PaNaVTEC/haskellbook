module Chapter9 where

myWordsChar :: Char -> String -> [String]
myWordsChar c a = go a []
  where go [] acc = acc
        go b acc  = go (dropUntil b) $ acc ++ [takeWhile (/= c) b]
        dropUntil b = drop 1 $ dropWhile (/= c) b

myWords :: String -> [String]
myWords = myWordsChar ' '

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]
tupleGen = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

eftBool :: Bool -> Bool -> [Bool]
eftBool = eftEnum

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftEnum

eftInt :: Int -> Int -> [Int]
eftInt = eftEnum

-- generic one!
eftEnum :: (Enum a, Ord a) => a -> a -> [a]
eftEnum from t = go from []
  where go f acc
          | f == t = acc ++ [f]
          | f < t = go (succ f) (acc ++ [f])
          | otherwise = acc

multOfThree :: [Integer]
multOfThree = [x | x <- [1..30] , x `mod` 3 == 0]

filterArticles :: String -> [String]
filterArticles sentence = filter articleWords sentenceInWords
  where sentenceInWords = myWords sentence
        articles = ["the", "a"]
        articleWords = (\x -> not $ elem x articles)

ownZip :: [a] -> [b] -> [(a, b)]
ownZip a1 b1 = go a1 b1 []
  where
    go :: [a] -> [b] -> [(a,b)] -> [(a,b)]
    go [] bs acc = acc
    go as [] acc = acc
    go as bs acc = go (tail as) (tail bs) $ makeTuple as bs : acc
    makeTuple as bs = (,) (head as) (head bs)
