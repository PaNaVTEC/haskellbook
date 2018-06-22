{-# LANGUAGE InstanceSigs #-}
module Chapter18 where

import           Chapter16                (List (Cons, Nil))
import           Chapter17                (concat')
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure b = Second b
  (<*>) (Second f) (Second b) = Second $ f b
  (<*>) (First f) (First b)   = First b
  (<*>) (First f) _           = First f
  (<*>) _ (First b)           = First b

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) f  = First a
  (>>=) (Second b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, firstGen), (2, secondGen)]

firstGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
firstGen = First <$> arbitrary

secondGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
secondGen = Second <$> arbitrary

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

type SumQB = Sum (Int) (String, Int, String)

data Nope a = NopeDotJpg

instance Functor Nope where fmap _ _ = NopeDotJpg
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) fa = f <$> fa
instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

type IdentityQB = Identity (String, Int, String)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> return $ Identity a)
instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) l f   = concat' (fmap f l)

type ListQB = List (Int, String, Int)
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return $ Cons a Nil

instance Eq a => EqProp (List a) where (=-=) = eq

j :: Monad m => m (m a) -> m a
j mma = mma >>= (\ma -> ma >>= pure)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = f <$> ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= (\a -> mf >>= (\f -> pure (f a)))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh arr f = sequence $ f <$> arr

flipType :: Monad m => [m a] -> m [a]
flipType arr = meh arr id

testLaws :: IO ()
testLaws = do
  putStrLn "Sum a b"
  quickBatch $ monad (undefined :: SumQB)
  quickBatch $ applicative (undefined :: SumQB)
  quickBatch $ monad (undefined :: IdentityQB)
  quickBatch $ monad (undefined :: ListQB)
