module Chapter6 where

import Data.List

i :: Num a => a
--i :: a
i = 1

f :: Float
-- f :: Num a => a
-- f :: Fractional a => a
--f :: RealFrac a => a
f = 1.0

--freud :: a -> a
--freud :: Ord a => a -> a
freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
--sigmund :: Int a => a -> a
sigmund x = myX

jung :: Ord a => [a] -> a
--jung :: [Int] -> Int
jung xs = head (sort xs)

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

--mySort :: [Char] -> Char
--mySort = sort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = b == f a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f _ a = f a


