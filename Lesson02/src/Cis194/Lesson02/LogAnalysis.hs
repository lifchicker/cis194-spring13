{-# OPTIONS_GHC -Wall #-}
module Cis194.Lesson02.LogAnalysis where
import Cis194.Lesson02.Log

parseMessage :: String -> LogMessage
parseMessage x = case words x of
  "I":t:xs -> LogMessage Info (read t) (unwords xs)
  "W":t:xs -> LogMessage Warning (read t) (unwords xs)
  "E":s:t:xs -> LogMessage (Error (read s)) (read t) (unwords xs)
  _ -> Unknown x

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) t@(Node lt lm@(LogMessage _ tts _) rt)
  | ts < tts = Node (insert m lt) lm rt
  | ts > tts = Node lt lm (insert m rt)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:[]) = Node Leaf x Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt lm rt) = (inOrder lt) ++ [lm] ++ (inOrder rt)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error s) _ m) :[])
  | s > 50 = [m]
  | otherwise = []
whatWentWrong (x:[]) = []
whatWentWrong (x:xs) = (whatWentWrong (x:[])) ++ (whatWentWrong xs)
