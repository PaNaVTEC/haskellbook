module Chapter15 where

import           Data.Monoid
import qualified Data.Semigroup  as S
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

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- optionalGen
  return (First' a)

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = frequency [(2, nadaGen) , (5, onlyGen)]

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
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Semigroups
data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity semigroup
newtype Identity a = Identity a deriving (Eq, Show)
instance S.Semigroup (Identity a) where
  a <> _ = a
instance (Arbitrary a, Monoid a) => Arbitrary (Identity a) where
  arbitrary = do
    i <- arbitrary
    return $ Identity i
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Tuple semigroup
data Two a b = Two a b deriving (Eq, Show)
instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a S.<> c) (b S.<> d)
instance (Arbitrary a, Arbitrary b, Monoid a, Monoid b, S.Semigroup a, S.Semigroup b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y
instance Arbitrary a => Arbitrary (S.First a) where
  arbitrary = do
    x <- arbitrary
    return (S.First x)
type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

-- Thriple semigroup
data Three a b c = Three a b c deriving (Eq, Show)
instance (S.Semigroup a, S.Semigroup b, S.Semigroup c) => S.Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = case (Two a b) S.<> (Two d e) of
    (Two _a _b) -> Three _a _b (c S.<> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z
type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

-- Four semigroup
data Four a b c d = Four a b c d deriving (Eq, Show)
instance ( S.Semigroup a
         , S.Semigroup b
         , S.Semigroup c
         , S.Semigroup d) => S.Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) =
    case ((Two a b) S.<> (Two e f), (Two c d) S.<> (Two g h)) of
    ((Two _a _b), (Two _c _d)) -> (Four _a _b _c _d)

instance (Arbitrary a
         ,Arbitrary b
         ,Arbitrary c
         ,Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    a <- arbitrary
    return $ Four x y z a

type FourAssoc a b c d = (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance S.Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance S.Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj $ a || b

data Or a b = Fst a | Snd b deriving (Eq, Show)
instance S.Semigroup (Or a b) where
  a@(Snd _) <> _ = a
  _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    g <- frequency [(5, return $ Fst x), (5, return $ Snd y)]
    return g

type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }
instance S.Semigroup b => S.Semigroup (Combine a b) where
  a <> b = Combine $ (unCombine a) S.<> (unCombine b)
type CombineAssoc a b = (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool

newtype Comp a = Comp { unComp :: a -> a }
instance S.Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g
type CompAssoc a = Comp a -> Comp a -> Comp a -> Bool

-- Law check
semigroupAssoc :: (S.Semigroup m, Eq m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

mainSemigroup :: IO ()
mainSemigroup = do
  _ <- quickCheck (semigroupAssoc :: TrivAssoc)
  _ <- quickCheck (semigroupAssoc :: (IdentityAssoc (Sum Int)))
  _ <- quickCheck (semigroupAssoc :: (TwoAssoc (First Int) (First Int)))
  _ <- quickCheck (semigroupAssoc :: (ThreeAssoc (S.First Int) (S.First Int) (S.First Int)))
  _ <- quickCheck (semigroupAssoc :: (FourAssoc (S.First Int) (S.First Int) (S.First Int) (S.First Int)))
  _ <- quickCheck (semigroupAssoc :: (OrAssoc Int Int))
  return ()

data Validation a b = Failure' a | Success' b deriving (Eq, Show)
instance S.Semigroup a => S.Semigroup (Validation a b) where
  a@(Success' _) <> _ = a
  _ <> a@(Success' _) = a
  (Failure' a) <> (Failure' b) = Failure' (a S.<> b)

validationCheck :: IO ()
validationCheck = do
  let failure :: String -> Validation String Int
      failure = Failure'
  let success :: Int -> Validation String Int
      success = Success'
  print $ success 1 S.<> failure "blah"
  print $ failure "woot" S.<> failure "blah"
  print $ success 1 S.<> success 2
  print $ failure "woot" S.<> success 2

-- Monoid instances
instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (S.<>)

instance (S.Semigroup a, S.Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (S.<>)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (S.<>)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (S.<>)

instance (S.Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (S.<>)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (S.<>)

newtype Mem s a = Mem { runMem :: s -> (a, s) }
instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend a b = Mem $ \iniState -> let
    (obj, nextState) = runMem a $ iniState
    (obj', nextState') = runMem b $ nextState
    in
    (obj <> obj', nextState')

f' :: Mem Integer [Char]
f' = Mem $ \s -> ("hi", s + 1)

mainMem :: IO ()
mainMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

type MemLeftId s a = Mem s a -> Mem s a -> Bool
type MemRightId s a = Mem s a -> Mem s a -> Bool
type MemAssoc s a = Mem s a -> Mem s a -> Mem s a -> Bool

mainMonoid :: IO ()
mainMonoid = do
  _ <- quickCheck (monoidAssoc :: TrivAssoc)
  _ <- quickCheck (monoidAssoc :: (IdentityAssoc (Sum Int)))
  _ <- quickCheck (monoidAssoc :: (TwoAssoc (First Int) (First Int)))
  return ()
