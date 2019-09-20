module Cis194.Lesson08.Party (main, spec) where

import Test.Hspec
import Test.QuickCheck
      
import Cis194.Lesson08.Party
  
main :: IO ()
main = hspec spec
  
spec :: Spec
spec = do
  