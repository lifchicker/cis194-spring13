-- CIS 194 Homework 3

module Cis194.Lesson03.Golf where

import Data.List

-- for each index j (started from 1)
-- generate the list of pairs (i, a), where i - index of element a in original list (something like [(1, "A"), (2, "B")])
-- for each pair filter only the elements for which i devided by j without the remainder
skips :: [a] -> [[a]]
skips xs = [([a | (i, a) <- (zip [1..n] xs), i `mod` j == 0]) | let n = length xs, j <- [1..n]]

-- create a tuples with (i, i+1, i+2) elements from original list
-- filter elements which is local maximums
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [b | (a, b, c) <- zip3 xs (tail xs) (tail (tail xs)), (a < b) && (b > c)]
