module Arith3Broken where

main :: IO ()
main = do
  print $ 1 + 2
  print 10
  print $ negate $ -1
  print ((+) 0 blah)
  where blah = negate 1

--f :: Int -> String
--f = undefined
--g :: String -> Char
--g = undefined
--h :: Int -> Char
--h a = g $ f a
--
--data A
--data B
--data C
--q :: A -> B
--q = undefined
--w :: B -> C
--w = undefined
--e :: A -> C
--e a = w $ q a

--data X
--data Y
--data Z
--xz :: X -> Z
--xz = undefined
--yz :: Y -> Z
--yz = undefined
--xform :: (X, Y) -> (Z, Z)
--xform (a, b) = (,) xz a yz b

--munge :: (x -> y) -> (y -> (w, z)) -> x -> w
--munge xToY yToWandZ x = fst $ yToWandZ $ xToY x