-- CIS 194 Homework 3

module Cis194.Lesson03.Golf where

import Data.List

mapIndex :: [a] -> [(Int, a)]
mapIndex [] = []
mapIndex xs = zip [1..(length xs)] xs

genList j (i, a) = if i `mod` j == 0 then [a] else []

genLists :: [a] -> [(Int, a)] -> [[[a]]]
genLists xs ys = map (\x -> (map (genList x) ys)) [1..(length xs)]

-- for each index i collect all the letters with index j where j/i==0
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips xs = map (\x -> concat x) (genLists xs (mapIndex xs))
