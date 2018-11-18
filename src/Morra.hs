{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Maybe             (maybe, isJust)
import           System.Random          (randomRIO)
import           Text.Read              (readMaybe)
import           Control.Monad (void)
import           Data.Bool (bool)

main :: IO ()
main = undefined

playVsPlayer :: IO ()
playVsPlayer = runMode initialState vsPlayer
  where
    initialState = GameState
      (PlayerState zeroScore Odd "Player 1")
      (PlayerState zeroScore Even "Player 2")

playVsComputer :: IO ()
playVsComputer = runMode initialState vsComputer
  where
    initialState = GameState
      (PlayerState zeroScore Odd "Player 1")
      (PlayerState zeroScore Even "Computer")

runMode state mode = void $ untilM' (const False) state runGame
  where runGame s = snd <$> runStateT mode s

zeroScore = Score 0

newtype Score = Score Int deriving (Num, Eq, Show)
data PlayerState = PlayerState {
  playerScore :: Score,
  playerMark :: EvenOrOdd,
  playerName :: String
}
data GameState = GameState { p1 :: PlayerState, p2 :: PlayerState }

newtype ValidInput = ValidInput { unInput :: Int } deriving (Eq, Show)
mkValidInput :: Int -> Maybe ValidInput
mkValidInput i = bool Nothing (Just $ ValidInput i) (i `elem` [0..10])

readPlayerInput :: MonadIO m => m (Maybe ValidInput)
readPlayerInput = maybe Nothing mkValidInput . readMaybe <$> liftIO getLine

vsComputer :: (MonadState GameState m, MonadIO m) => m ()
vsComputer = do
  _pInput <- untilM isJust readPlayerInput
  _cInput <- liftIO $ mkValidInput <$> randomRIO (0, 10)
  gameLogic _pInput _cInput

vsPlayer :: (MonadState GameState m, MonadIO m) => m ()
vsPlayer = do
  _p1Input <- untilM isJust readPlayerInput
  _p2Input <- untilM isJust readPlayerInput
  gameLogic _p1Input _p2Input

gameLogic :: (MonadIO m, MonadState GameState m) =>
  Maybe ValidInput -> Maybe ValidInput -> m ()
gameLogic _p1Input _p2Input = do
  printInfo _p1Input _p2Input =<< get
  maybe (return ())
    decideWhoWins
    (calculateEvenOrOdd _p1Input _p2Input)
  where
    decideWhoWins t = do
      printWhoWins t =<< get
      put . incScore t =<< get
      printGameState =<< get
    printWhoWins t GameState { p1=_p1, p2=_p2 } = liftIO . putStrLn $ bool
      (playerName _p2 ++ " wins")
      (playerName _p1 ++ " wins")
      (playerMark _p1 == t)

printInfo _p1Input _p2Input GameState { p1=_p1, p2=_p2 } = liftIO $ do
  putStr $ playerName _p1 ++ " is " ++ show (playerMark _p1) ++ ", "
  putStrLn $ playerName _p2 ++ " is " ++ show (playerMark _p2)

  putStrLn $ playerName _p1 ++ ": " ++ show _p1Input
  putStrLn $ playerName _p2 ++ ": " ++ show _p2Input

incScore Even gameState@GameState{ p1=_p1 } =
  gameState { p1 = _p1 { playerScore = playerScore _p1 + 1 }}
incScore Odd gameState@GameState{ p2=_p2 } =
  gameState { p2 = _p2 { playerScore = playerScore _p2 + 1 }}

printGameState :: MonadIO m => GameState -> m ()
printGameState GameState { p1=_p1, p2=_p2 } = do
  liftIO $ print $ playerScore _p1
  liftIO $ print $ playerScore _p2

calculateEvenOrOdd :: Maybe ValidInput -> Maybe ValidInput -> Maybe EvenOrOdd
calculateEvenOrOdd playerInput computerInput = (\a b -> mkEvenOdd $ unInput a + unInput b) <$> playerInput <*> computerInput

data EvenOrOdd = Even | Odd deriving (Show, Eq)
mkEvenOdd :: Int -> EvenOrOdd
mkEvenOdd i = if even i then Even else Odd

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p ma = (\a -> if p a then return a else untilM p ma) =<< ma

untilM' :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
untilM' p _a ama = (\a -> if p a then return a else untilM' p a ama) =<< ama _a
