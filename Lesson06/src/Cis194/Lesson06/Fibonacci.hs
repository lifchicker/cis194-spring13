-- CIS 194 Homework 6

module Cis194.Lesson06.Fibonacci where

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..] ]

fibs2 :: [Integer]
fibs2 = 0 : 1 : (fib2 0 1)
  where 
    fib2 x y = xy : fib2 y xy
      where xy = x + y
