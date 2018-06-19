{-# LANGUAGE InstanceSigs #-}
module Chapter17 where

import           Chapter16
import           Control.Applicative
import           Data.List                (elemIndex)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: (Maybe Integer, Maybe Integer)
tupled = (,) y z

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'
--maxed = Just max' <*> x <*> y'

xs :: [Integer]
xs = [1,2,3]

ys :: [Integer]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x' <*> y'')

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f a = Constant (getConstant a)

instance Monoid a => Applicative (Constant a) where
  pure :: a1 -> Constant a a0
  pure _ = Constant mempty

  (<*>) :: Constant a (a1 -> b) -> Constant a a1 -> Constant a b
  (<*>) (Constant f) (Constant a) = Constant $ mappend f a

ex1 :: Maybe [Char]
ex1 = const <$> Just "Hello" <*> pure "World"

ex2 :: Maybe (Integer, Integer, [Char], [Integer])
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

-- idLaw = pure id <*> v = v
-- compositionLaw (.) <*> u <*> v <*> w = u <*> (v <*> w)

instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Nil

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) l l' = concat' $ fmap (\a -> fmap (\f -> f a) l) l'

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap (\a -> f a) as

take' :: Int -> List a -> List a
take' n l = go n l
  where
    go _ Nil        = Nil
    go 0 _          = Nil
    go n (Cons h t) = Cons h (go (n - 1) (t))

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs' =-= ys' = xs'' `eq` ys''
    where xs'' = let (ZipList' l) = xs'
                in take' 3000 l
          ys'' = let (ZipList' l) = ys'
                in take' 3000 l

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Functor ZipList' where
  fmap f (ZipList' xss) = ZipList' $ fmap f xss

instance Applicative ZipList' where
  pure :: a -> ZipList' a
  pure a = ZipList' (Cons a Nil)

  (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
  (<*>) (ZipList' lf) (ZipList' la) = ZipList' $ flatMap (\a -> (\f -> f a) <$> lf) la

data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation' e) where
  fmap f (Success' a) = Success' $ f a
  fmap f (Failure' a) = Failure' a

instance Monoid e => Applicative (Validation' e) where
  pure :: a -> Validation' e a
  pure a = Success' a

  (<*>) :: Validation' e (a -> b) -> Validation' e a -> Validation' e b
  (<*>) (Failure' fa) (Failure' a) = Failure' $ mappend fa a
  (<*>) (Failure' fa) (Success' a) = Failure' $ fa
  (<*>) (Success' fa) (Failure' a) = Failure' $ a
  (<*>) (Success' fa) (Success' a) = Success' $ fa a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
  arbitrary = frequency [(3, failureGen), (3, successGen)]

failureGen :: Arbitrary e => Gen (Validation' e a)
failureGen = do
  e <- arbitrary
  return $ Failure' e

successGen :: Arbitrary a => Gen (Validation' e a)
successGen = do
  a <- arbitrary
  return $ Success' a

instance (Eq a, Eq e) => EqProp (Validation' e a) where
  (=-=) = eq

-- Can be checked with:
-- :set -XTypeApplications
-- :t pure @[]

-- type []
-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]

-- type IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- type IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- check with:
-- :t pure @((,) _)
-- type (,) a
-- pure :: Monoid w => a -> (w, a) -- needed to do mempty
-- (<*>) :: Monoid w => (w, a -> b) -> (w, a) -> IO (w, b)

-- check with:
-- :t (<*>) @((->) _)
-- type (-> e)
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)

instance Applicative Pair where
  pure :: a -> Pair a
  pure a = Pair a a

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance Monoid a => Applicative (Two a) where
  pure :: a1 -> Two a a1
  pure b = Two mempty b

  (<*>) :: Two a (a' -> b) -> Two a a' -> Two a b
  (<*>) (Two a fb) (Two a' b) = Two (a <> a') (fb b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

type VQB = Validation'
  (String, Data.Monoid.Sum Int, Data.Monoid.Sum Int)
  (String, Data.Monoid.Sum Int, Data.Monoid.Sum Int)
type PQB = Pair (String, Int, Int)
type TQB = Two
  (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Product Int)
  (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Product Int)

testLaws :: IO ()
testLaws = do
  putStrLn "Validation"
  quickBatch $ applicative (undefined :: VQB)
  putStrLn ""

  putStrLn "Pair"
  quickBatch $ applicative (undefined :: PQB)
  putStrLn ""

  putStrLn "Two"
  quickBatch $ applicative (undefined :: TQB)
  putStrLn ""
