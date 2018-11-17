{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config = Config { counts :: IORef (M.Map Text Integer), prefix :: Text }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handle = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k newValue m, newValue)
  where newValue = maybe 0 (+1) (M.lookup k m)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = prefix config <> unprefixed
    counter <- lift . lift $ readIORef $ counts config
    let (newCounter, newInteger) = bumpBoomp key' counter
    lift . lift $ writeIORef (counts config) newCounter
    html $ mconcat [ "", TL.pack $ show newInteger, "" ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
