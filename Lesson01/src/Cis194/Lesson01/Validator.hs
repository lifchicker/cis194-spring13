module Cis194.Lesson01.Validator ( validate ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = n : []
  | otherwise = (toDigits (n `div` 10)) ++ (n `mod` 10 : [])


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = x:[]
doubleEveryOther (x:y:xs)
  | length(xs) `mod` 2 == 0 = [2*x, y] ++ (doubleEveryOther xs)
  | otherwise = [x, 2*y] ++ (doubleEveryOther xs)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + (sumDigits xs)
  | otherwise = (x `div` 10) + (x `mod` 10) + (sumDigits xs)


validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0
