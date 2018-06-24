module Chapter20 where

--import           Data.Foldable
import           Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum fa = getSum $ foldMap Sum fa

product :: (Foldable t, Num a) => t a -> a
product fa = getProduct $ foldMap Product fa

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a' ta = foldr (\a b -> a == a' || b) False ta

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum ta = foldr minC Nothing ta
  where
    minC :: Ord a => a -> Maybe a -> Maybe a
    minC a ma = (\a' -> a `min` a') <$> ma

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum ta = foldr maxC Nothing ta
  where
    maxC :: Ord a => a -> Maybe a -> Maybe a
    maxC a ma = (\a' -> a `max` a') <$> ma

null' :: (Foldable t) => t a -> Bool
null' ta = foldr (\_ _ -> True) False ta

length' :: (Foldable t) => t a -> Int
length' ta = foldr (\_ b -> b + 1) 0 ta

toList' :: (Foldable t) => t a -> [a]
toList' ta = foldr (\a b -> [a] ++ b) [] ta

fold' :: (Foldable t, Monoid m) => t m -> m
fold' tm = foldMap id tm

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr (\a b -> (f a) <> b) mempty ta

data Constant a b = Constant b
instance Foldable (Constant a) where
  foldr f b (Constant a) = f a b

data Two a b = Two a b
instance Foldable (Two a) where
  foldr f b (Two _ b') = f b' b

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldr f b (Three _ _ c) = f c b

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four a b = Four a b b b
instance Foldable (Four a) where
  foldMap f (Four _ b b' b'') = f b <> f b' <> f b''

filterF :: (Applicative f
           , Foldable t
           , Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p ta = foldMap (\a -> if p a then pure a else mempty) ta
