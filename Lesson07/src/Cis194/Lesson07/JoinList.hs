-- CIS 194 Homework 7

module Cis194.Lesson07.JoinList where

import Data.List

import Cis194.Lesson07.Sized


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (mappend (tag l) (tag r)) l r

getListSize :: (Sized b, Monoid b) => JoinList b a -> Int
getListSize = (getSize . size . tag)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ x) = Just x
indexJ i jl | i >= (getListSize jl) = Nothing
indexJ i (Append _ l r)
  | i < ls = indexJ i l
  | otherwise = indexJ (i - ls) r
  where ls = getListSize l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 xs = xs
dropJ 1 x@(Single _ _) = Empty
dropJ i x@(Append _ l r)
  | i > (getListSize x)  = Empty
  | i == (getListSize l) = r
  | i < (getListSize l) = (dropJ i l) +++ r
  | otherwise     = (dropJ (i - (getListSize l)) r)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ i xs | i == (getListSize xs) = xs
takeJ i (Append _ l r)
  | i <= ls = takeJ i l
  | i > ls = l +++ (takeJ (i - ls) r)
  where ls = getListSize l
