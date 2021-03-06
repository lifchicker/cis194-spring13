module Cis194.Lesson07.JoinListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
    
import Cis194.Lesson07.JoinList
import Cis194.Lesson07.Sized
import Cis194.Lesson07.Scrabble

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let jl = (Single (Size 1) 0) +++ (Single (Size 1) 1)
  let jl2 = (Single (Size 1) 0) +++ (Single (Size 1) 1) +++ (Single (Size 1) 2) +++ (Single (Size 1) 3)
  describe "indexJ" $ do
    it "returns Nothing, if the index is out of bounds" $ do
      (indexJ 2 jl) `shouldBe` Nothing
    it "finds the JoinList element at the specified index" $ do
      (indexJ 1 jl) `shouldBe` (Just 1)
  describe "dropJ" $ do
    it "drops the first n elements from a JoinList" $ do
      (dropJ 1 jl) `shouldBe` (Single (Size 1) 1)
      (dropJ 2 jl) `shouldBe` Empty
      (dropJ 3 jl2) `shouldBe` (Single (Size 1) 3)
  describe "takeJ" $ do
    it "returns the first n elements of a JoinList, dropping all other elements" $ do
      (takeJ 0 jl) `shouldBe` Empty
      (takeJ 1 jl) `shouldBe` (Single (Size 1) 0)
  describe "scoreLine" $ do
    it "calculate score of JoinList" $ do
      (scoreLine "yay " +++ scoreLine "haskell!") `shouldBe` Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
