module Cis194.Lesson01.TowersOfHanoiSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Lesson01.TowersOfHanoi

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "should return a list of moves to be performed to move the stack of discs from the first peg to the second" $ do
      (hanoi 2 "a" "b" "c") `shouldBe`  [("a","c"), ("a","b"), ("c","b")]
