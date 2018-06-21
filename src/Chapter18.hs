{-# LANGUAGE InstanceSigs #-}
module Chapter18 where

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

testLaws :: IO ()
testLaws = do
  putStrLn "Sum a b"
  quickBatch $ monad (undefined :: SumQB)
  quickBatch $ applicative (undefined :: SumQB)
  putStrLn ""
