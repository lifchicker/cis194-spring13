-- CIS 194 Homework 3

module Cis194.Lesson03.Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = [([a | (i, a) <- (zip [1..n] xs), i `mod` j == 0]) | let n = length xs, j <- [1..n]]
