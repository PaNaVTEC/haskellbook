{-# LANGUAGE InstanceSigs #-}

module Chapter24 where

import           Control.Applicative
import           Test.Hspec
import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = choice $ char <$> ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> many parseDigit

base10Integer' :: Parser Integer
base10Integer' = parseNegative <|> base10Integer
  where
    parseNegative = try (char '-') >> negate <$> base10Integer

-- The rest of the exercises are in AltParsing.hs, LeanParsers.hs, Fractions.hs

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- skipMany $ try $ char '1' >> char '-'

  _ <- skipMany $ char '('
  planArea <- digitLength 3
  _ <- skipMany $ char ')'
  _ <- try $ whiteSpace
  _ <- skipMany $ char '-'

  exchange <- digitLength 3
  _ <- try $ whiteSpace
  _ <- skipMany $ char '-'

  lineNumber <- digitLength 4

  return $ PhoneNumber planArea exchange lineNumber
  where
    digitLength :: Int -> Parser Int
    digitLength n = read <$> count n digit

test :: IO ()
test = hspec $ do
  describe "Parse phone" $ do
    it "with hyphens" $ do
      show (parseString parsePhone mempty "123-456-7890")
        `shouldBe` show (Success (PhoneNumber 123 456 7890))

    it "without hyphens" $ do
      show (parseString parsePhone mempty "1234567890")
        `shouldBe` show (Success (PhoneNumber 123 456 7890))

    it "with parenthesys, hypens and spaces" $ do
      show (parseString parsePhone mempty "(123) 456-7890")
        `shouldBe` show (Success (PhoneNumber 123 456 7890))

    it "with parenthesys" $ do
      show (parseString parsePhone mempty "1-123-456-7890")
        `shouldBe` show (Success (PhoneNumber 123 456 7890))
