module Chapter8 where

mainy :: IO ()
mainy = print $ dividedBy 2 0

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 0 = 0
recursiveSum n = n + (recursiveSum n - 1)

timesWithRecSum :: (Integral a) => a -> a -> a
timesWithRecSum 0 _ = 0
timesWithRecSum t m = m + timesWithRecSum (t - 1) m

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> (DividedResult, a)
dividedBy _ 0 = (DividedByZero, 0)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (Result count, n)
         | otherwise = go (n - d) d (count + 1)

mc91 :: (Ord a, Num a) => a -> a
mc91 n | n > 100  = n - 10
       | n <= 100 = 91

