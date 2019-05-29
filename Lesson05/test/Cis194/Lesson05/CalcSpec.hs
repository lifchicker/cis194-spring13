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
    describe "evalStr" $ do
      it "evaluates arithmetic expressions given as a String, \
      \producing Nothing for inputs which are not well-formed expressions, \
      \and Just n for well-formed inputs that evaluate to n" $ do
        evalStr "(2+3)*4" `shouldBe` Just 20
        evalStr "2+3*4" `shouldBe` Just 14
        evalStr "2+3*" `shouldBe` Nothing
    describe "Expr" $ do
      it "should evaluate functions to objects" $ do
        (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
