{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chapter11 where

import           Data.Int

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
