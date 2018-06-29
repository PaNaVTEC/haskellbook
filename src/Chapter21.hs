{-# LANGUAGE InstanceSigs #-}

module Chapter21 where

import           Chapter16
import           Chapter17                (concat')
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

testLaws :: IO ()
testLaws = do
  putStrLn "Sum a b"
  quickBatch $ traversable (undefined :: IdentityAT)
