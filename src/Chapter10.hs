module Chapter10 where

fibonacci :: Integral a => a -> a
fibonacci n | n <= 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

fibs :: Integral a => a -> [a]
fibs n = [fibonacci x | x <- [1..n]]

fibsScan :: Integral a => a -> [a]
fibsScan n = scanl (\b a -> fibonacci a + b) 1 [1..n]

factorialScan :: Integral a => a -> [a]
factorialScan n = scanl (\b a -> a * b) 1 [2..n]

-- Stop and vowels

svsCombinations :: [(Char, Char, Char)]
svsCombinations = stops >>= (\s -> vowels >>= (\v -> map (\s2 -> (,,) s v s2) stops))
  where stops = "pbtdkg"
        vowels = "aeiou"

myOrFold :: [Bool] -> Bool
myOrFold = foldl (||) False

myAndFold :: (a -> Bool) -> [a] -> Bool
myAndFold f = foldr (\a b -> f a || b) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a arr = any (\b -> a==b) arr

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold n xs = foldr (||) False (fmap (== n) xs)

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f as = foldr (\a b -> [f a] ++ b) [] as

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f as = foldr (\a b -> if f a then  b else a : b) [] as

squish :: [[a]] -> [a]
squish as = foldl (\a b -> a ++ b) [] as

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f as = squish $ map (\a -> f a) as

squishAgain :: [[a]] -> [a]
squishAgain as = squishMap (\a -> a) as


