{-# LANGUAGE InstanceSigs #-}

module Chapter26 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import qualified Control.Monad.State as ST
import Data.Functor.Identity

import Control.Monad.Trans.Maybe

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT rea) = EitherT $ (fmap . fmap) f rea

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure . pure $ a

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT ab) <*> (EitherT a) = EitherT $ (liftA2 . liftA2) ($) ab a

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  eita >>= ab = join $ ab <$> eita

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swapEither ma

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT amb) = amb >>= mapLR
  where
    mapLR (Left a) = amc a
    mapLR (Right b) = bmc b

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap ab (StateT smas) = StateT $ \s -> mapT <$> (smas s)
   where
     mapT (a, s) = (ab a, s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT smab) (StateT smas) = StateT $ \s -> do
    (a, s') <- smas s
    (ab, _) <- smab s
    return (ab a, s')

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= asmb = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (asmb a) s'

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift ma = EitherT $ ma >>= return . Right

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> ma >>= \a -> return (a, s)

rDec :: Num a => Reader a a
rDec = asks (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = asks show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  i <- ask
  _ <- liftIO $ putStrLn $ "Hi: "  ++ show i
  return $ i + 1

sPrintIncAccum :: (Num a, Show a) => ST.StateT a IO String
sPrintIncAccum = do
  i <- ST.get
  _ <- liftIO $ putStrLn $ "Hi: "  ++ show i
  ST.put (i + 1)
  return $ show i

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
