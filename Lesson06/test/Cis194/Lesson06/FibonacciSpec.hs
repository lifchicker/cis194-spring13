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
      take 30 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229]
  describe "fibs2" $ do
    it "more efficient implementation of infinite list of all Fibonacci numbers" $ do
      take 11 fibs2 `shouldBe` take 11 fibs1
  describe "streamRepeat" $ do
    it "generates a stream containing infinitely many copies of the given element" $ do
      show (streamRepeat 0) `shouldBe` "[0,0,0,0,0,0,0,0,0,0]"
  describe "streamMap" $ do
    it "applies a function to every element of a Stream" $ do
      show (streamMap (+1) $ streamRepeat 1) `shouldBe` "[2,2,2,2,2,2,2,2,2,2]"
  describe "streamFromSeed" $ do
    it "generates a Stream from a “seed” of type a, which is the first element of the stream, and an “unfolding rule” of type a -> a \
    \ which specifies how to transform the seed into a new seed, to be \
    \ used for generating the rest of the stream" $ do
      show (streamFromSeed (+1) 0) `shouldBe` "[1,2,3,4,5,6,7,8,9,10]"
  describe "nats" $ do
    it "contains the infinite list of natural numbers 0, 1, 2, ..." $ do
      show nats `shouldBe` "[0,1,2,3,4,5,6,7,8,9]"
