module Cis194.Lesson02.LogAnalysisSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Cis194.Lesson02.Log
import Cis194.Lesson02.LogAnalysis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "is supposed to parse log message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
  describe "parse" $ do
    it "is supposed to parse many lines of messages" $ do
      parse "E 65 8 Bad pickle-flange interaction detected\nW 5 Flange is due for a check-up\nI 7 Out for lunch, back in two time steps" `shouldBe` [LogMessage (Error 65) 8 "Bad pickle-flange interaction detected", LogMessage Warning 5 "Flange is due for a check-up", LogMessage Info 7 "Out for lunch, back in two time steps"]
  describe "insert" $ do
    it "suppose to insert message in correct place in sorted tree" $ do
      insert (LogMessage Info 2 "b") (Node Leaf (LogMessage (Error 1) 10 "c") Leaf) `shouldBe` (Node (Node Leaf (LogMessage Info 2 "b") Leaf) (LogMessage (Error 1) 10 "c") Leaf)
      insert (LogMessage Info 4 "d") (Node Leaf (LogMessage Info 3 "c") Leaf) `shouldBe` (Node Leaf (LogMessage Info 3 "c") (Node Leaf (LogMessage Info 4 "d") Leaf))
  describe "build" $ do
    it "suppose to build a MessageTree from a list of LogMessages" $ do
      build [(LogMessage Info 2 "b"), (LogMessage Info 3 "c")] `shouldBe` (Node (Node Leaf (LogMessage Info 2 "b") Leaf) (LogMessage Info 3 "c") Leaf)
  describe "inOrder" $ do
    it "suppose to make a sorted list of LogMessage out of MessageTree" $ do
      inOrder (Node Leaf (LogMessage Info 3 "c") (Node Leaf (LogMessage Info 4 "d") Leaf)) `shouldBe` [(LogMessage Info 3 "c"), (LogMessage Info 4 "d")]
  describe "whatWentWrong" $ do
    it "suppose to returns a list of the messages corresponding to any errors with a severity of 50 or greater, sorted by timestamp" $ do
      ((whatWentWrong . inOrder . build . parse) "I 6 Completed armadillo processing\nI 1 Nothing to report\nE 99 10 Flange failed!\nI 4 Everything normal\nI 11 Initiating self-destruct sequence\nE 70 3 Way too many pickles\nE 65 8 Bad pickle-flange interaction detected\nW 5 Flange is due for a check-up\nI 7 Out for lunch, back in two time steps\nE 20 2 Too many pickles\nI 9 Back from lunch") `shouldBe` [ "Way too many pickles", "Bad pickle-flange interaction detected", "Flange failed!"]
