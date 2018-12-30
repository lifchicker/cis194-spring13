-- CIS 194 Homework 3

module Cis194.Lesson03.Golf where

import Data.List

genList j (i, a) = if i `mod` j == 0 then [a] else []

-- for each index i collect all the letters with index j where j/i==0
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips xs = [(concat (map (genList x) (zip [1..(length xs)] xs))) | x <- [1..(length xs)]]
