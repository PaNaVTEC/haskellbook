import           Data.List           (sort)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen

half :: Fractional a => a -> a
half x = x / 2

fractionalGen :: Gen Float
fractionalGen = (arbitrary :: Gen Float)

prop_HalfIdentity :: Property
prop_HalfIdentity =
  forAll fractionalGen (\a -> a == half (a * 2))

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x >= y)

listOfInt :: Gen [Int]
listOfInt = listOf (arbitrary :: Gen Int)

prop_AlwaysSorted :: Property
prop_AlwaysSorted = forAll listOfInt (\a -> listOrdered (sort a) == True)

thriplesOfInts :: Gen (Int, Int, Int)
thriplesOfInts = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_Associative :: Property
prop_Associative = forAll thriplesOfInts associativityHolds
  where associativityHolds (x, y, z) = x + (y + z) == (x + y) + z

tupleOfInts :: Gen (Int, Int)
tupleOfInts = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_Commutative :: Property
prop_Commutative = forAll tupleOfInts commutativityHolds
  where commutativityHolds (x, y) = x + y == y + x

main :: IO ()
main = hspec $ do
  describe "digitToWord $ do" $ do
    it "returns zero for 0" $ do
      quickCheck prop_HalfIdentity
    it "lists are sorted" $ do
      quickCheck prop_AlwaysSorted
    it "associativity holds" $ do
      quickCheck prop_Associative
    it "commutativity holds" $ do
      quickCheck prop_Commutative
