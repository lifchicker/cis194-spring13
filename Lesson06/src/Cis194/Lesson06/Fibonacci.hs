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

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show xs = show $ take 10 $ streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream y (streamFromSeed f y)
  where y = f x

nats :: Stream Integer
nats = Stream 0 (streamFromSeed (+1) 0)
