module Cis194.Lesson06.FibonacciSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
  
import Cis194.Lesson06.Fibonacci
  
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec
  
spec :: Spec
spec = do
  describe "fib" $ do
    it "fib n computes the nth Fibonacci number Fn" $ do
      fib 10 `shouldBe` 55
  describe "fibs1" $ do
    it "defines the infinite list of all Fibonacci numbers" $ do
      take 11 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
