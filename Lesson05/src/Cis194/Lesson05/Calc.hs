-- CIS 194 Homework 5

module Cis194.Lesson05.Calc where

import Cis194.Lesson05.ExprT

class Evaluable e where
  calc :: e -> Integer

instance Evaluable ExprT where
  calc (Lit x) = x
  calc (Add x y) = (eval x) + (eval y)
  calc (Mul x y) = (eval x) * (eval y)

eval :: ExprT -> Integer
eval = calc