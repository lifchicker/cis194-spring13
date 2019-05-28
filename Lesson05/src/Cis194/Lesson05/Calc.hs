-- CIS 194 Homework 5

module Cis194.Lesson05.Calc where

import Cis194.Lesson05.ExprT
import Cis194.Lesson05.Parser

class Evaluable e where
  calc :: e -> Integer

instance Evaluable ExprT where
  calc (Lit x) = x
  calc (Add x y) = (eval x) + (eval y)
  calc (Mul x y) = (eval x) * (eval y)

eval :: ExprT -> Integer
eval = calc

class EvaluableMaybe e where
  evalMaybe :: e -> Maybe Integer

instance (Evaluable e) => EvaluableMaybe (Maybe e) where
  evalMaybe Nothing = Nothing
  evalMaybe (Just x) = Just (calc x)

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . (parseExp Lit Add Mul)
