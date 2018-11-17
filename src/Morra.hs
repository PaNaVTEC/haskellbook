{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Maybe             (fromJust, isJust)
import           System.Random          (randomRIO)
import           Text.Read              (readMaybe)

main :: IO ()
main = const () <$> foo

newtype Score = Score { unScore :: Int } deriving (Num, Eq, Show)
data GameState = GameState {
  playerScore   :: Score,
  computerScore :: Score
} deriving (Eq, Show)

newtype ValidInput = ValidInput { unInput :: Int } deriving (Eq, Show)
mkValidInput :: Int -> Maybe ValidInput
mkValidInput i = if i `elem` [0..10] then Just $ ValidInput i else Nothing

readPlayerInput :: MonadIO m => m (Maybe ValidInput)
readPlayerInput = maybe Nothing mkValidInput . readMaybe <$> liftIO getLine

game :: (MonadState GameState m, MonadIO m) => m ()
game = do
  playerInput <- untilInputIsCorrect
  computerInput <- liftIO $ mkValidInput <$> randomRIO (0 :: Int, 10)

  liftIO . putStrLn $ "P: " ++ show playerInput
  liftIO . putStrLn $ "C: " ++ show computerInput
  return ()

untilInputIsCorrect :: MonadIO m => m (Maybe ValidInput)
untilInputIsCorrect = untilM isJust readPlayerInput

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p ma = (\a -> if p a then return a else untilM p ma) =<< ma
