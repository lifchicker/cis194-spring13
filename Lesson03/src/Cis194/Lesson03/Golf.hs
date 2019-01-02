-- CIS 194 Homework 3

module Cis194.Lesson03.Golf where

import Data.List

-- for each index j (started from 1)
-- generate the list of pairs (i, a), where i - index of element a in original list (something like [(1, "A"), (2, "B")])
-- for each pair filter only the elements for which i devided by j without the remainder
skips :: [a] -> [[a]]
skips xs = [([a | (i, a) <- (zip n xs), i `mod` j == 0]) | let n = [1..(length xs)], j <- n]

-- create a tuples with (i, i+1, i+2) elements from original list
-- filter elements which is local maximums
localMaxima :: [Int] -> [Int]
localMaxima xs = [b | (a, b, c) <- zip3 xs (tail xs) (tail (tail xs)), (a < b) && (b > c)]

-- for each number, calculate how many of them in input array
-- starting from max value go throug all elements and compare
--   if the element appear >= value times, then print "*"
--   otherwise print " "
counts xs = [length $ filter (== i) xs | i <- [0..9]]
histogram :: [Int] -> String
histogram xs = intercalate "\n" ([concat (map (\x -> if x >= i then "*" else " ") c) | let c = counts xs, i <- (reverse [1..(maximum c)])] ++ ["=========="] ++ ["0123456789"])
