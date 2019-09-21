{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cis194.Lesson08.Party where

import Data.Monoid
import Data.Semigroup  

import Cis194.Lesson08.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = ef }) gl@(GL xs x) = GL (e:xs) (ef + x)

instance Semigroup GuestList where 
  -- can't come up with anything "correct" right now, will leave till later moment when it'll be used
  -- two options: 
  -- 1. just add all element from one to another (wrong)
  -- 2. add without duplicates, re-calculate fun (because newly added elements might reduce fun)
  (<>) = undefined

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ le) r@(GL _ re) = if le > re then l else r
