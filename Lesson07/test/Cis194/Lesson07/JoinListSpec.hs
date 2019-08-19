module Cis194.Lesson07.JoinListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
    
import Cis194.Lesson07.JoinList
import Cis194.Lesson07.Sized

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "indexJ" $ do
    it "returns Nothing, if the index is out of bounds" $ do
      let i = 1
      let jl = (Single (Size 1) 0)
      (indexJ i jl) `shouldBe` Nothing
    it "finds the JoinList element at the specified index" $ do
      let i = 1
      let jl = (Single (Size 1) 0) +++ (Single (Size 1) 1)
      (indexJ i jl) `shouldBe` (Just 1)
      