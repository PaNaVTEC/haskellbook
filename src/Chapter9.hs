module Chapter9 where

myWordsChar :: Char -> String -> [String]
myWordsChar c a = go a []
  where go [] acc = acc
        go b acc = go (dropUntil b) $ acc ++ [takeWhile (/= c) b]
        dropUntil b = drop 1 $ dropWhile (/= c) b

myWords :: String -> [String]
myWords = myWordsChar ' '

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]
tupleGen = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
