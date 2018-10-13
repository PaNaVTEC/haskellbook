{-# LANGUAGE InstanceSigs #-}

module Chapter26 where

import Control.Applicative

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT rea) = EitherT $ (fmap . fmap) f rea

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure . pure $ a

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT ab) <*> (EitherT a) = EitherT $ (liftA2 . liftA2) ($) ab a
