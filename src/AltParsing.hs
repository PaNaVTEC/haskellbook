{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AltParsing where

import           Control.Applicative
import           Text.RawString.QQ   (r)
import           Text.Trifecta

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)
