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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ x) = Just x
indexJ i jl | i >= ((getSize . size . tag) jl) = Nothing
indexJ i (Append _ l r)
  | i < ls = indexJ i l
  | otherwise = indexJ (i - ls) r
  where ls = (getSize . size . tag) l
