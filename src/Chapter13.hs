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
