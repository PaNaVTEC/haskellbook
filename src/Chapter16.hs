module Chapter16 where

import           Test.QuickCheck
import           Test.QuickCheck.Function

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

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Or a b = First a | Second b deriving (Eq, Show)
instance Functor (Or a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap ( g. f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

--- Functor instances
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

main :: IO ()
main = do
  _ <- quickCheck (functorCompose' :: IntFC)
  _ <- quickCheck (functorCompose' :: IdentityFC)
  _ <- quickCheck (functorCompose' :: PairFC)
  _ <- quickCheck (functorCompose' :: TwoFC)
  return ()
