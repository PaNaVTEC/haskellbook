{-# LANGUAGE InstanceSigs #-}

module Chapter21 where

import           Chapter16
import           Chapter17
import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Foldable Identity where
  foldMap :: (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse :: Functor f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity a) = Identity <$> (f a)

instance Eq a => EqProp (Identity a) where (=-=) = eq
type IdentityAT = Identity (String, Int, String)

instance Foldable (Constant a) where
  foldMap :: Monoid m => (b -> m) -> Constant a b -> m
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse :: Applicative f => (a1 -> f b) -> Constant a a1 -> f (Constant a b)
  traverse f (Constant a) = Constant <$> (pure a)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq
type ConstantAT = Identity (String, Int, String)

data Optional a = Nada | Yep a deriving (Show, Eq)
instance Functor Optional where
  fmap f Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap f (Yep a) = f a
  foldMap f Nada    = mempty

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f (Yep a) = Yep <$> f a
  traverse f Nada    = pure Nada

instance Eq a => EqProp (Optional a) where (=-=) = eq
type OptionalAT = Optional (String, Int, String)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ Yep a), (1, return Nada)]

instance Foldable List where
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap f (Cons a t) = (f a) <> foldMap f t
  foldMap f Nil        = mempty

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f (Cons a t) = liftA2 Cons (f a) (traverse f t)
  traverse f Nil        = pure Nil

instance Eq a => EqProp (List a) where (=-=) = eq
type ListAT = List (String, Int, String)

testLaws :: IO ()
testLaws = do
  putStrLn "Identity"
  quickBatch $ traversable (undefined :: IdentityAT)
  putStrLn "Constant"
  quickBatch $ traversable (undefined :: ConstantAT)
  putStrLn "Optional"
  quickBatch $ traversable (undefined :: OptionalAT)
