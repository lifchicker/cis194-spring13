module Cis194.Lesson01.ValidatorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Lesson01.Validator
    
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "validate" $ do
    it "is supposed to return True for valid credit card number, False otherwise" $ do
      4012888888881881 `shouldSatisfy` validate
      4012888888881882 `shouldNotSatisfy` validate
    