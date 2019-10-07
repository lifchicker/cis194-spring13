module Cis194.Lesson08.PartySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Tree

import Cis194.Lesson08.Party
import Cis194.Lesson08.Employee
  
main :: IO ()
main = hspec spec

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

spec :: Spec
spec = do
  let empA = Emp "A" 1
  let empB = Emp "B" 2
  describe "glCons" $ do
    it "adds an Employee to the GuestList (updating the cached Fun score appropriately)" $ do
      glCons empA (GL [] 0) `shouldBe` GL [empA] 1
  describe "moreFun" $ do
    it "takes two GuestLists and returns whichever one of them is more fun, i.e. has the higher fun score" $ do
      moreFun (GL [empA] 1) (GL [empA, empB] 3) `shouldBe` (GL [empA, empB] 3)
  describe "combineGLs" $ do
    it "employee (the boss of some division) and the optimal guest list for each subdivision under him" $ do
      combineGLs empA [(GL [empB] 2)] `shouldBe` GL [empB, empA] 3
  describe "maxFun" $ do
    it "which takes a company hierarchy as input and outputs a fun-maximizing guest list" $ do
      maxFun testCompany `shouldBe` GL [] 0
