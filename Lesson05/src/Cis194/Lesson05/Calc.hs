{-# LANGUAGE FlexibleInstances #-}
-- CIS 194 Homework 5

module Cis194.Lesson05.Calc where

import Cis194.Lesson05.ExprT
import Cis194.Lesson05.Parser
import qualified Cis194.Lesson05.StackVM as StackVM (Program, stackVM, StackExp(..), StackVal(..))
import qualified Data.Map as M

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

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Integer where
  lit x = x
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = if x <= 0 then False else True
  add = (&&)
  mul = (||)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (if x >= y then x else y)
  mul (MinMax x) (MinMax y) = MinMax (if x <= y then x else y)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = VVar

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul
  
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \m -> Just x
  add f g = \m -> case ((f m), (g m)) of 
    (Just x, Just y) -> Just (x + y)
    _ -> Nothing
  mul f g = \m -> case ((f m), (g m)) of 
    (Just x, Just y) -> Just (x * y)
    _ -> Nothing

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
