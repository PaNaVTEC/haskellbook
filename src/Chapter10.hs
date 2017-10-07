module Chapter10 where

fibonacci :: Integral a => a -> a
fibonacci n | n <= 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

fibs :: Integral a => a -> [a]
fibs n = [fibonacci(x) | x <- [1..n]]

fibsScan :: Integral a => a -> [a]
fibsScan n = scanl (\b a -> fibonacci(a) + b) 1 [1..n]

factorialScan :: Integral a => a -> [a]
factorialScan n = scanl (\b a -> a * b) 1 [2..n]

-- Stop and vowels

svsCombinations :: [(Char, Char, Char)]
svsCombinations = stops >>= (\s -> vowels >>= (\v -> map (\s2 -> (,,) s v s2) stops))
  where stops = "pbtdkg"
        vowels = "aeiou"

myOrFold :: [Bool] -> Bool
myOrFold = foldl (||) False
