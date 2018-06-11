{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

import           Test.QuickCheck
import           Test.QuickCheck.Function

ave :: Maybe String
ave = Just "ave"

n :: Maybe String
n = Nothing

w :: Maybe String
w = Just "woohoo"

lms :: [Maybe [Char]]
lms = [ave, n, w]

replaceWithP :: a -> Char
replaceWithP = const 'p'

a :: Char
a = replaceWithP lms

-- fmap :: Functor f => (a -> b) -> f a -> f b
b :: [Char]
b = fmap replaceWithP lms

-- :t fmap . fmap
-- fmap . fmap :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)

c :: [Maybe Char]
c = (fmap . fmap) replaceWithP lms

-- :t fmap . fmap . fmap
--fmap . fmap . fmap :: (Functor f3, Functor f2, Functor f1) =>
--(a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
d :: [Maybe [Char]]
d = (fmap . fmap . fmap) replaceWithP lms

-- Reducing (fmap . fmap)
-- fmap :: (a -> b) -> F a -> F b
-- . :: (b -> c) -> (a -> b) -> a -> c
-- (.) fmap :: Functor f => (a1 -> a2 -> b) -> a1 -> f a2 -> f b

-- Taking . and replacing with fmap . fmap
-- (b -> c) -> (a -> b) -> a -> c

fa :: [Int]
fa = fmap (+1) $ read "[1]" :: [Int]

fb :: Maybe [[Char]]
fb = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

fc :: Integer -> Integer
fc = (*2) . (\x -> x - 2)

fd :: Int -> [Char]
fd x = fmap ((return '1' ++) . show) (\x -> [x, 1..3]) $ x

fe :: IO Integer
fe = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123" ++) . show) ioi
     in fmap (*3) changed

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Or a b = First a | Second b deriving (Eq, Show)
instance Functor (Or a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap ( g. f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

--- Functor instances
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

type Four'FC = Four' Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Four' a a' b b'

-- Can't Implement Functor for Trivial because the kind of Functor is * -> * and trivial is *

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a
type PossibilyFC = Possibly Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(3, yeppersGen), (1, lolNopeGen)]

yeppersGen :: Arbitrary a => Gen (Possibly a)
yeppersGen = do
  a <- arbitrary
  return $ Yeppers a

lolNopeGen :: Arbitrary a => Gen (Possibly a)
lolNopeGen = return LolNope

data Sum a b = First' a | Second' b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap f (Second' a) = Second' (f a)
  fmap _ (First' a)  = First' a

type SumFC = Sum Int Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(3, firstGen), (1, secondGen)]

firstGen :: Arbitrary a => Gen (Sum a b)
firstGen = do
  a <- arbitrary
  return $ First' a

secondGen :: Arbitrary b => Gen (Sum a b)
secondGen = do
  a <- arbitrary
  return $ Second' a

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' $ f a
  fmap f (True' a)  = True' $ f a

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = frequency [(3, falseGen), (1, trueGen)]

falseGen :: Arbitrary a => Gen (BoolAndSomethingElse a)
falseGen = arbitrary >>= \a -> pure (False' a)

trueGen :: Arbitrary a => Gen (BoolAndSomethingElse a)
trueGen = arbitrary >>= \a -> pure (True' a)

type BoolAndSomethingElseFC = BoolAndSomethingElse Int -> IntToInt -> IntToInt -> Bool

data Company a b c = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap _ (Something b)  = Something (b)
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a
instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

data Parappa f g a = Parappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (Parappa f' g) = Parappa (fmap f f') (fmap f g)

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething f' g) = IgnoreSomething f' (fmap f g)

data List a = Nil | Cons a (List a)
instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a t) = Cons (f a) (fmap f t)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read f')   = Read (\s -> f (f' s))

main :: IO ()
main = do
  _ <- quickCheck (functorCompose' :: IntFC)
  _ <- quickCheck (functorCompose' :: IdentityFC)
  _ <- quickCheck (functorCompose' :: PairFC)
  _ <- quickCheck (functorCompose' :: TwoFC)
  _ <- quickCheck (functorCompose' :: ThreeFC)
  _ <- quickCheck (functorCompose' :: FourFC)
  _ <- quickCheck (functorCompose' :: Four'FC)
  _ <- quickCheck (functorCompose' :: PossibilyFC)
  _ <- quickCheck (functorCompose' :: SumFC)
  _ <- quickCheck (functorCompose' :: BoolAndSomethingElseFC)
  return ()
