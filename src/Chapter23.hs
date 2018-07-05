{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.Monoid
import           System.Random

type Iso a b = (a -> b, b -> a)

--newtype Sum a = Sum { getSum :: a }
sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  _ -> error $ "not possible"

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes ::  StateT StdGen Identity (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = fst $ rollsCountLogged n g

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go acc count gen
      | acc >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (acc + die) (fst count + 1, snd count ++ [intToDie $ die]) nextGen

rollsToGetNRandom :: IO Int
rollsToGetNRandom = (rollsToGetN 20 . mkStdGen) <$> randomIO

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> do
    let (a, s') = g s
    (f a, s')
