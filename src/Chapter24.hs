{-# LANGUAGE InstanceSigs #-}

module Chapter24 where

import           Control.Applicative
import           Data.Int
import           Data.Word
import           Numeric             (readHex)
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

testPhoneNumbers :: IO ()
testPhoneNumbers = hspec $ do
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

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

parseIp :: Parser IPAddress
parseIp = do
  f <- parseSegment
  _ <- char '.'
  s <- parseSegment
  _ <- char '.'
  t <- parseSegment
  _ <- char '.'
  ff <- parseSegment
  return $ IPAddress
    $ f * 256 ^ 3
    + s * 256 ^ 2
    + t * 256 ^ 1
    + ff * 256 ^ 0
  where
    parseSegment :: Parser Word32
    parseSegment = read <$> some digit

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)
parseIp6 :: Parser IPAddress6
parseIp6 = do
  f <- (\a b c d -> (a + b + c + d) :: Word64) <$> parseSegment <*> parseSegment <*> parseSegment <*> parseSegment
--  t <- (\a b c d -> a + b + c + d) <$> parseSegment <*> parseSegment <*> parseSegment <*> parseSegment
  return $ IPAddress6 f 1
  where
    parseSegment :: Parser Integer
    parseSegment = try (constP hexSegment separator)
      <|> const 0 <$> separator
    hexSegment :: Parser Integer
    hexSegment = fst . head . readHex <$> (some alphaNum)
    separator :: Parser ()
    separator = try (const () <$> char ':') <|> eof

constP :: (Parsing m, Monad m) => m a -> m b -> m a
constP ma mb = ma >>= (\a -> const a <$> mb)

ip6ToInteger :: IPAddress6 -> Integer
ip6ToInteger (IPAddress6 q r) = (toInteger q) * (toInteger (maxBound :: Word)) + (toInteger r)

testIp6 :: IO ()
testIp6 = hspec $ do
  describe "Tests ip6 addresses" $ do
    let ip1 = "FE80::0202:B3FF:FE1E:8329"
        ip2 = "0:0:0:0:0:ffff:cc78:f"
        ip3 = "ABCD:EF01:2345:6789:ABCD:EF01:2345:6789"

    it ("can parse " ++ ip1) $ do
      let (Success x) = parseString parseIp6 mempty ip1
      ip6ToInteger x `shouldBe` 338288524927261089654163772891438416681

    it ("can parse " ++ ip2) $ do
      let (Success x) = parseString parseIp6 mempty ip2
      ip6ToInteger x `shouldBe` 281474112159759

    it ("can parse " ++ ip3) $ do
      let (Success x) = parseString parseIp6 mempty ip3
      ip6ToInteger x `shouldBe` 228367255721259907113560617832333443072
