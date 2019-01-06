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
    describe "skips" $ do
      it "The output of skips is a list of lists. The first list in the output should\
      \ be the same as the input list. The second list in the output should\
      \ contain every second element from the input list. . . and the nth list in\
      \ the output should contain every nth element from the input list." $ do
        skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
        skips [1] `shouldBe` [[1]]
        skips [True,False] `shouldBe` [[True,False], [False]]
        -- skips [] `shouldBe` []
    describe "localMaxima" $ do
      it "A local maximum of a list is an element of the list which is strictly\
      \ greater than both the elements immediately before and after it" $ do
        localMaxima [2,9,5,6,1] `shouldBe` [9,6]
        localMaxima [2,3,4,1,5] `shouldBe` [4]
        localMaxima [1,2,3,4,5] `shouldBe` []
    describe "histogram" $ do
      it "takes as input a list of Integers between 0 and 9 (inclusive),\
      \ and outputs a vertical histogram showing how many of each number\
      \ were in the input list" $ do
        histogram [1,1,1,5] `shouldBe` " *        \n *        \n *   *    \n==========\n0123456789"
        histogram  [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789"
  