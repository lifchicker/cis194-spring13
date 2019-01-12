-- CIS 194 Homework 4

module Cis194.Lesson04.Wholemeal where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x-2)*y) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2(n `div` 2)
  | otherwise = fun2(3 * n + 1)

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n = (foldr (+) 0) $ (:) ((fun2' . (+) 1 . (*) 3 . head . filter (/=1) . dropWhile (even)) $ iterate (\x -> x `div` 2) n) ((takeWhile (even)) $ iterate (\x -> x `div` 2) n)
