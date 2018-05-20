import           Test.Hspec

import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

half :: Fractional a => a -> a
half x = x / 2

fractionalGen :: Gen Float
fractionalGen = (arbitrary :: Gen Float)

prop_HalfIdentity =
  forAll fractionalGen (\a -> a == half (a * 2))

main :: IO ()
main = hspec $ do
  describe "digitToWord $ do" $ do
    it "returns zero for 0" $ do
      quickCheck prop_HalfIdentity
