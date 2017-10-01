module Main where

import           Chapter8
import           WordNumber

main :: IO ()
main = print $ WordNumber.wordNumber 1000

--main = Chapter8.mainy
--main = print third
--main = putStrLn("Hello world Haskell")
--main = print plusTwo
-- where plusTwo = 2 + 2

first :: Int
first = x * 3 + y
  where x = 3
        y = 1000

second :: Int
second = x * 5
  where x = 10 * 5 + y
        y = 10


third = z / x + y
  where x = 7
        y = negate x
        z = y * 10

--myAbs :: Integer -> Integer
--myAbs x | x >= 0 = x
--        | _ = -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = (,) ((,) (snd x) (snd y)) $ (,) (fst x) (fst y)


x = (+)
fff xs = w `x` 1
  where w = length xs


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else -x

-- makeTuple :: (a,b) -> (c,d) -> ((b,d), (a,c))
-- makeTuple a b = (,) a b
