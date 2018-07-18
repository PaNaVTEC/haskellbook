{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

import Control.Applicative
import Data.Bifunctor

newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ (liftA2 . liftA2) ($) fgab fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap am (Compose fga) = (foldMap . foldMap) am fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative f' => (a -> f' b) -> Compose f g a -> f' (Compose f g b)
  traverse afb (Compose fga) = undefined

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap ab cd (Deux a c) = Deux (ab a) (cd c)

data Const a b = Const a
instance Bifunctor Chapter25.Const where
  bimap :: (a -> b) -> (c -> d) -> Chapter25.Const a c -> Chapter25.Const b d
  bimap ab cd (Chapter25.Const a) = Chapter25.Const $ ab a

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap :: (a1 -> b) -> (c -> d) -> Drei a a1 c -> Drei a b d
  bimap a1b cd (Drei a a1 c) = Drei a (a1b a1) (cd c)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap :: (a1 -> b) -> (c -> d) -> SuperDrei a a1 c -> SuperDrei a b d
  bimap a1b cd (SuperDrei a1 b) = SuperDrei a1 (a1b b)

data Semidrei a b c = Semidrei a
instance Bifunctor (Semidrei a) where
  bimap ab cd (Semidrei a) = Semidrei a

data Quadriceps a b c d = Quazzz a b c d
instance Bifunctor (Quadriceps a b) where
  bimap :: (a1 -> b1) -> (c -> d) -> Quadriceps a b a1 c -> Quadriceps a b b1 d
  bimap a1b1 cd (Quazzz a b a1 c) = Quazzz a b (a1b1 a1) (cd c)

data Either a b = Left a | Right b
instance Bifunctor Chapter25.Either where
  bimap :: (a -> b) -> (c -> d) -> Chapter25.Either a c -> Chapter25.Either b d
  bimap ab cd (Chapter25.Left a) = Chapter25.Left $ ab a
  bimap ab cd (Chapter25.Right c) = Chapter25.Right $ cd c
