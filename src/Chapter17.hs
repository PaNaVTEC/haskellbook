{-# LANGUAGE InstanceSigs #-}
module Chapter17 where

import           Data.List (elemIndex)

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

  pure :: a1 -> Constant a a1
  pure _ = Constant mempty

  (<*>) :: Constant a (a1 -> b) -> Constant a a1 -> Constant a b
  (<*>) (Constant f) (Constant a) = Constant $ mappend f a
