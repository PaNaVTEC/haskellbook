{-# LANGUAGE InstanceSigs #-}

module Chapter24 where

import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = choice $ char <$> ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> many parseDigit

-- The rest of the exercises are in AltParsing.hs, LeanParsers.hs, Fractions.hs
