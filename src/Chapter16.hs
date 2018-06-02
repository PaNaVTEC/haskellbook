module Chapter16 where

ave :: Maybe String
ave = Just "ave"

n :: Maybe String
n = Nothing

w :: Maybe String
w = Just "woohoo"

lms :: [Maybe [Char]]
lms = [ave, n, w]

replaceWithP :: a -> Char
replaceWithP = const 'p'

a :: Char
a = replaceWithP lms

-- fmap :: Functor f => (a -> b) -> f a -> f b
b :: [Char]
b = fmap replaceWithP lms

-- :t fmap . fmap
-- fmap . fmap :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)

c :: [Maybe Char]
c = (fmap . fmap) replaceWithP lms

-- :t fmap . fmap . fmap
--fmap . fmap . fmap :: (Functor f3, Functor f2, Functor f1) =>
--(a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
d :: [Maybe [Char]]
d = (fmap . fmap . fmap) replaceWithP lms

-- Reducing (fmap . fmap)
-- fmap :: (a -> b) -> F a -> F b
-- . :: (b -> c) -> (a -> b) -> a -> c
-- (.) fmap :: Functor f => (a1 -> a2 -> b) -> a1 -> f a2 -> f b

-- Taking . and replacing with fmap . fmap
-- (b -> c) -> (a -> b) -> a -> c

fa :: [Int]
fa = fmap (+1) $ read "[1]" :: [Int]

fb :: Maybe [[Char]]
fb = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

fc :: Integer -> Integer
fc = (*2) . (\x -> x - 2)

fd :: Int -> [Char]
fd x = fmap ((return '1' ++) . show) (\x -> [x, 1..3]) $ x

fe :: IO Integer
fe = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123" ++) . show) ioi
     in fmap (*3) changed
