module Chapter15 where

import           Data.Monoid
import           Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where

  mempty = Nada

  mappend Nada Nada         = Nada
  mappend (Only a) (Only b) = Only $ a <> b
  mappend (Only a) _        = Only a
  mappend _ (Only a)        = Only a

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

  mappend a b = case (getFirst' a, getFirst' b) of
    (_, Only _) -> b
    _           -> a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

firstGen :: (Arbitrary a) => Gen (First' a)
firstGen = do
  a <- optionalGen
  return (First' a)

optionalGen :: (Arbitrary a) => Gen (Optional a)
optionalGen = frequency
  [
    (2, nadaGen)
  , (5, onlyGen)]

nadaGen :: (Arbitrary a) => Gen (Optional a)
nadaGen = return Nada

onlyGen :: (Arbitrary a) => Gen (Optional a)
onlyGen = do
  a <- arbitrary
  return $ Only a

-- Monoid Laws

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty <> a) == a

monoidAssoc :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
