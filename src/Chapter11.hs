{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chapter11 where

import           Data.Char
import           Data.Int
import           Data.List
import           Data.String

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'us |TakeYourChancesUnited deriving (Eq, Show)
data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = undefined

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Show, Eq, TooMany)
-- Without the language extension:
-- instance TooMany Goats where tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
  tooMany (i, _) = i > 42

instance TooMany (Int, Int) where
  tooMany (c1, c2) = c1 + c2 > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (c1, c2) = tooMany (c1 + c2)

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- cardinality 2 + 2 = 4

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStrill | Mac | Windows deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux, OpenBSDPlusNevermindJustBSDStrill, Mac, Windows ]

allLanguages :: [ProgLang]
allLanguages = [ Haskell, Agda, Idris, PureScript ]

allProgrammers :: [Programmer]
allProgrammers = concat $
  map (\os -> map (\lang -> Programmer os lang) allLanguages) allOperatingSystems

data BinaryTree a =
  Leaf | Node (BinaryTree a ) a (BinaryTree a ) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

map' :: (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf                = Leaf
map' f (Node left a right) = Node (map' f left) (f a) (map' f right)

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf                = []
postOrder (Node left a right) = postOrder left ++ postOrder right ++ [a]

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf             = b
foldTree f b (Node _ a right) = foldTree f (f a b) right

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _            = True
isSubseqOf _ []            = False
isSubseqOf (sh:st) (th:tt) | sh == th = isSubseqOf st tt
isSubseqOf src (_:tt)      = isSubseqOf src tt

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map capitalizeTuple (words sentence)
  where
    capitalizeTuple word@(x : xs) = (word, (toUpper x) : xs)
    capitalizeTuple []            = ([], [])
