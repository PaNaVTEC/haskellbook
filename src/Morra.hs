{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Maybe             (maybe, isJust)
import           System.Random          (randomRIO)
import           Text.Read              (readMaybe)

main :: IO ()
main = fst <$> runStateT game (GameState (Score 0) (Score 0))

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
  _pInput <- untilM isJust readPlayerInput
  _cInput <- liftIO $ mkValidInput <$> randomRIO (0, 10)

  liftIO $ do
    putStrLn "Player is odds, computer is evens."
    putStrLn $ "P: " ++ show _pInput
    putStrLn $ "C: " ++ show _cInput

  maybe
    (return ())
    (\eo -> do
        liftIO $ case eo of
          Even -> putStrLn "- C wins"
          Odd -> putStrLn "- P wins"
        cState <- get
        put $ incScore cState eo
    )
    (calculateEvenOrOdd _pInput _cInput)

  where
    incScore gameState Even =
      gameState { computerScore = computerScore gameState + 1 }
    incScore gameState Odd =
      gameState { playerScore = playerScore gameState + 1 }

calculateEvenOrOdd :: Maybe ValidInput -> Maybe ValidInput -> Maybe EvenOrOdd
calculateEvenOrOdd playerInput computerInput = (\a b -> mkEvenOdd $ unInput a + unInput b) <$> playerInput <*> computerInput

data EvenOrOdd = Even | Odd
mkEvenOdd :: Int -> EvenOrOdd
mkEvenOdd i = if even i then Even else Odd

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p ma = (\a -> if p a then return a else untilM p ma) =<< ma
