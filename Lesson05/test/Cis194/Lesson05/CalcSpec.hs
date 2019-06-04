module Cis194.Lesson05.CalcSpec (main, spec) where

  import Test.Hspec
  import Test.QuickCheck
    
  import Cis194.Lesson05.ExprT
  import Cis194.Lesson05.Calc
  import qualified Cis194.Lesson05.StackVM as StackVM (stackVM, StackExp(..), StackVal(..))
    
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
    describe "testExp" $ do
      it "should evaluate for Integer, Bool, MinMax and Mod7 types" $ do
        (testExp :: Maybe Integer) `shouldBe` Just (-7)
        (testExp :: Maybe Bool) `shouldBe` Just True
        (testExp :: Maybe MinMax) `shouldBe` Just (MinMax 5)
        (testExp :: Maybe Mod7) `shouldBe` Just (Mod7 0)
    describe "stackVM" $ do
      it "should execute program" $ do
        StackVM.stackVM [StackVM.PushI 3, StackVM.PushI 5, StackVM.Add] `shouldBe` Right (StackVM.IVal 8)
    describe "compile" $ do
      it "should compile string to executable program" $ do
        compile "3 + 5" `shouldBe` Just [StackVM.PushI 3, StackVM.PushI 5, StackVM.Add]
