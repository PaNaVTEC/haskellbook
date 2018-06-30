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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, (Cons a) <$> arbitrary)
              , (1, return $ Nil)]

instance Foldable (Three a b) where
  foldMap :: (c -> m) -> Three a b c -> m
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse :: Applicative f => (c -> f c') -> Three a b c -> f (Three a b c')
  traverse f (Three a b c) = (Three a b) <$> f c

type ThreeAT = Three
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)

instance Eq a => EqProp (Data.Monoid.Sum a) where (=-=) = eq
instance Eq a => EqProp (Data.Monoid.Product a) where (=-=) = eq

data PairAB a b = PairAB a b deriving (Eq, Show)
instance Functor (PairAB a) where
  fmap :: (b -> c) -> PairAB a b -> PairAB a c
  fmap f (PairAB a b) = PairAB a (f b)

instance Foldable (PairAB a) where
  foldMap :: (b -> m) -> PairAB a b -> m
  foldMap f (PairAB _ b) = f b

instance Traversable (PairAB a) where
  traverse :: Applicative f => (b -> f c) -> PairAB a b -> f (PairAB a c)
  traverse f (PairAB a b) = (PairAB a) <$> f b
type PairABAT = PairAB
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)
instance (Arbitrary a, Arbitrary b) => Arbitrary (PairAB a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ PairAB a b
instance (Eq a, Eq b) => EqProp (PairAB a b) where (=-=) = eq

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap :: Monoid m => (b -> m) -> Big a b -> m
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse :: Applicative f => (b -> f c) -> Big a b -> f (Big a c)
  traverse f (Big a b b') = Big a <$> (f b) <*> (f b')

type BigAT = Big
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)
   (Data.Monoid.Sum Int, Data.Monoid.Product Int, Data.Monoid.Sum Int)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    frequency [(1, return $ Big a b b'), (1, return $ Big a b b)]
instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

testLaws :: IO ()
testLaws = do
  putStrLn "Identity"
  quickBatch $ traversable (undefined :: IdentityAT)
  putStrLn "Constant"
  quickBatch $ traversable (undefined :: ConstantAT)
  putStrLn "Optional"
  quickBatch $ traversable (undefined :: OptionalAT)
  putStrLn "List"
  quickBatch $ traversable (undefined :: ListAT)
  putStrLn "Three"
  quickBatch $ traversable (undefined :: ThreeAT)
  putStrLn "Pair"
  quickBatch $ traversable (undefined :: PairABAT)
  putStrLn "Big"
  quickBatch $ traversable (undefined :: BigAT)
