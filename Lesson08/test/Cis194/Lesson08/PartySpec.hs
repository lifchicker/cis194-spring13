module Cis194.Lesson08.PartySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
      
import Cis194.Lesson08.Party
import Cis194.Lesson08.Employee
  
main :: IO ()
main = hspec spec
  
spec :: Spec
spec = do
  describe "glCons" $ do
    it "adds an Employee to the GuestList (updating the cached Fun score appropriately)" $ do
      glCons (Emp "A" 1) (GL [] 0) `shouldBe` GL [(Emp "A" 1)] 1
  describe "moreFun" $ do
    it " takes two GuestLists and returns whichever one of them is more fun, i.e. has the higher fun score" $ do
      moreFun (GL [(Emp "A" 1)] 1) (GL [(Emp "A" 1), (Emp "B" 1)] 2) `shouldBe` (GL [(Emp "A" 1), (Emp "B" 1)] 2)
