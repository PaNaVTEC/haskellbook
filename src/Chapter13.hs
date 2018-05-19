module Chapter13 where

import           Control.Monad
import           Data.Char     (isAlpha, toLower)
import           System.Exit   (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (palindrome' line1) of
    True -> putStrLn "It's a palindrome"
    False -> do
      _ <- putStrLn "Nope!"
      exitSuccess

-- Madam I'm Adam
palindrome' :: String -> Bool
palindrome' sentence = filtered == reverse filtered
  where
    filtered = fmap toLower . filter isAlpha $ sentence

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age | name /= "" && age > 0 =
  Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
    " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  ageish <- getLine
  case mkPerson name (read ageish) of
    Left NameEmpty -> putStrLn "The introduced name is invalid"
    Left AgeTooLow -> putStrLn "Age to low"
    Left (PersonInvalidUnknown msg) -> putStrLn msg
    Right p -> do
      putStrLn "Yay ! Successfully got a person: "
      putStrLn . show $ p
