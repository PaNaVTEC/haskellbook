module Chapter12 where

import           Data.List

-- notThe "the" -> Nothing
-- notThe "woot" -> Just 'woot'
-- notThe "blahtheblah" -> Just 'blahtheblah'
notThe :: String -> Maybe String
notThe text | text == "the" = Nothing
notThe text | text == "The" = Nothing
notThe text = Just text

-- "The cow loves us" -> "a cow loves us"
replaceThe :: String -> String
replaceThe text = unwords $ map replace (words text)
  where
    replace :: String -> String
    replace word = maybe "a" id (notThe word)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = (go 0) $ words text
  where
    go :: Integer -> [String] -> Integer
    go acc []                = acc
    go acc ("the" : word : rest) = (toInt . startsWithVowel $ word) + (go acc rest) + acc
    go acc (_ : rest)            = acc + go acc rest
    startsWithVowel []     = False
    startsWithVowel (l: _) = isVowel l

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel l = elem l vowels

toInt :: Bool -> Integer
toInt i = if i == True then 1 else 0

countPredicate :: String -> (Char -> Bool) -> Int
countPredicate sentence f = sum . fmap inWord $ words sentence
  where
    inWord :: String -> Int
    inWord word = length . filter f $ word

countVowels :: String -> Int
countVowels sentence = countPredicate sentence isVowel

countConsonants :: String -> Int
countConsonants sentence = countPredicate sentence isConsonant

newtype Word' = Word' String deriving (Eq, Show)

isConsonant :: Char -> Bool
isConsonant l = notElem l vowels && elem l ['a'..'z'] || elem l ['a'..'Z']

mkWord :: String -> Maybe Word'
mkWord t = case (countVowels t > countConsonants t) of
  True  -> Nothing
  False -> Just $ Word' t

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Num a => Nat -> a
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

natToInteger' :: Num a => Nat -> a
natToInteger' nat = sum $ unfoldr toOne nat
  where
    toOne Zero     = Nothing
    toOne (Succ n) = Just (1, n)

integerToNat :: Int -> Maybe Nat
integerToNat i | i >= 0 = Just $ foldl (\b _ -> Succ b) Zero (replicate i 1)
integerToNat _ = Nothing

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing  = b
mayybe _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybe a id m

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList a = maybe [] (\b -> [b]) a

catMaybes :: [Maybe a] -> [a]
catMaybes arr = fmap extract . filter isJust $ arr
  where extract (Just a) = a

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe arr = case catMaybes arr of
  a | length a == length arr -> Just a
  _ -> Nothing

lefts' :: [Either a b] -> [a]
lefts' arr = foldr toLeft [] arr
  where
    toLeft (Left v) b = [v] ++ b
    toLeft _ b        = b

rights' :: [Either a b] -> [b]
rights' arr = foldr toLeft [] arr
  where
    toLeft (Right v) b = [v] ++ b
    toLeft _ b         = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' arr = (lefts' arr, rights' arr)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e =  either' (\_ -> Nothing) (Just . f) e

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f (f a))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing      -> []
  Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\b -> Just (b, b)) a

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing         -> Leaf
  Just (a, b, a') -> Node (unfold f a) b (unfold f a')

treeBuild :: Integer -> BinaryTree Integer
treeBuild i = unfold something i
  where
    something 0 = Nothing
    something a = Just (a - 1, a - 1, a - 1)
