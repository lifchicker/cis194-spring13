module Cis194.Lesson05.CalcSpec (main, spec) where

  import Test.Hspec
  import Test.QuickCheck
    
  import Cis194.Lesson05.ExprT
  import Cis194.Lesson05.Calc
    
  -- `main` is here so that this module can be run from GHCi on its own.  It is
  -- not needed for automatic spec discovery.
  main :: IO ()
  main = hspec spec
    
  spec :: Spec
  spec = do
    describe "eval" $ do
      it "evaluate the expression" $ do
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
