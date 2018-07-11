module SemVer where

import           Control.Applicative
import           Test.Hspec
import           Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

constP :: (Parsing m, Monad m) => m a -> m b -> m a
constP ma mb = ma >>= (\a -> const a <$> mb)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  rel <- parseNumberOrStringArray '-'
  meta <- parseNumberOrStringArray '+'
  _ <- eof
  return $ SemVer major minor patch rel meta

parseNumberOrStringArray :: Char -> Parser [NumberOrString]
parseNumberOrStringArray c = (try (char c) >> some parseNumberOrString)
  <|> const [] <$> eof

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
  p <- parseNOSS <|> parseNOSI
  _ <- skipMany $ char '.'
  return p
  where
    parseNOSS = (NOSS <$> (try (some letter) <?> "Tried some letters"))
    parseNOSI = (NOSI . read <$> (try (some digit) <?> "Tired some digits"))

test :: IO ()
test = hspec $ do
  describe "parseSemver" $ do
    it "parses major min patch" $ do
      show (parseString parseSemVer mempty "2.1.1")
        `shouldBe` show (Success (SemVer 2 1 1 [] []))
    it "parses major min patch release" $ do
      show (parseString parseSemVer mempty "1.0.0-x.7.z.92")
        `shouldBe` show (Success
                         (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []))
    it "parses full example" $ do
      show (parseString parseSemVer mempty "1.0.0-x.7.z.92+fullrelease")
        `shouldBe` show (Success
                         (SemVer 1 0 0
                          [NOSS "x", NOSI 7, NOSS "z", NOSI 92]
                          [NOSS "fullrelease"]))
