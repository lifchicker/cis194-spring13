module Cis194.Lesson04.WholemealSpec (main, spec) where

  import Test.Hspec
  import Test.QuickCheck
  
  import Cis194.Lesson04.Wholemeal
  
  -- `main` is here so that this module can be run from GHCi on its own.  It is
  -- not needed for automatic spec discovery.
  main :: IO ()
  main = hspec spec
  
  spec :: Spec
  spec = do
    describe "fun1'" $ do
      it "Should have the same output as fun1" $ do
        fun1' [1,3,4,5,6,7,8] `shouldBe` fun1 [1,3,4,5,6,7,8]
    describe "fun2'" $ do
      it "Should have the same output as fun2" $ do
        fun2' 1 `shouldBe` fun2 1
        fun2' 2 `shouldBe` fun2 2
        fun2' 3 `shouldBe` fun2 3
        fun2' 4 `shouldBe` fun2 4
        fun2' 5 `shouldBe` fun2 5
