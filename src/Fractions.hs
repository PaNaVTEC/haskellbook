{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

shouldWork = "1/2"
shouldAlsoWork = "2/1"
alsoBad = "10"
badFraction = "1/0"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

constP :: (Parsing m, Monad m) => m a -> m b -> m a
constP ma mb = ma >>= (\a -> const a <$> mb)

customEof :: (Parsing m, Monad m) => m a -> m a
customEof ma = constP ma eof

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
