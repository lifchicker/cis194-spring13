module Cis194.Lesson07.ScrabbleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
      
import Cis194.Lesson07.Scrabble
  
main :: IO ()
main = hspec spec
  
spec :: Spec
spec = do
  describe "score" $ do
    it "implement the tile scoring values as shown at http://www.thepixiepit.co.uk/scrabble/rules.html;" $ do
      (scoreString "abc") `shouldBe` Score 7
    it "should score any characters not mentioned (punctuation, spaces, etc.) should be given zero points" $ do
      (scoreString ",.-+=_ ") `shouldBe` 0
 