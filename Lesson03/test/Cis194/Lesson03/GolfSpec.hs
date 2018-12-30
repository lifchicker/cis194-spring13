module Cis194.Lesson03.GolfSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Cis194.Lesson03.Golf

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
