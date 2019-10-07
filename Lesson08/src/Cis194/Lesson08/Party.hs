{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cis194.Lesson08.Party where

import Data.Monoid
import Data.Semigroup  

import Data.Tree

import Cis194.Lesson08.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = ef }) (GL xs x) = GL (e:xs) (ef + x)

instance Semigroup GuestList where 
  (<>) (GL lxs ltf) (GL rxs rtf) = GL (lxs ++ rxs) (ltf + rtf)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ le) r@(GL _ re) = if le > re then l else r

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold = undefined

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e@(Emp { empFun = ef }) = foldr mappend (GL [e] ef)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e@(Emp { empFun = ef }) = foldr (\(llgl, lrgl) (rlgl, rrgl) -> (mappend llgl rlgl, mappend lrgl rrgl)) ((GL [e] ef), mempty)

maxFun :: Tree Employee -> GuestList
maxFun xs = moreFun (fst gl) (snd gl)
  where gl = treeFold nextLevel (mempty, mempty) xs
