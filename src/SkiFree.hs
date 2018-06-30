{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module SkiFree where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)
instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
         => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a)
         => EqProp (S n a) where
  (S x y) =-= (S p q) =
         (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap :: (a -> b) -> S n a -> S n b
  fmap f (S n a) = S (f <$> n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap :: Monoid m => (a -> m) -> S n a -> m
  foldMap f (S n a) = f a <> foldMap f n

instance Traversable n => Traversable (S n) where
  traverse :: Applicative f => (b -> f c) -> S n b -> f (S n c)
  traverse f (S n b) = liftA2 S (traverse f n) (f b)

main = sample' (arbitrary :: Gen (S [] Int))
