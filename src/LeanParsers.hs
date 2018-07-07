module LeanParsers where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

--one :: CharParsing m => m Char
one :: Parser Char
one = char '1'

one' :: Parser b
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwoEof :: Parser ()
oneTwoEof = oneTwo >> eof

testParse :: Show a => Parser a -> String -> IO ()
testParse p s = print $ parseString p mempty s

oneTwoOrThree :: Parser String
oneTwoOrThree = choice [string "123", string "12", string "1"]

customString :: String -> Parser String
--customString toParse = sequence $ char <$> toParse
customString = sequence . fmap char

main :: IO ()
main = do
  testParse oneTwoEof "12" -- "123" will fail because is not eof
  testParse oneTwoOrThree "1234" -- Parses until 3
--  parseString oneTwoEof mempty "123"
