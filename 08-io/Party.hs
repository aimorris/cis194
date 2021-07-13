{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee ( GuestList(..), Employee(empFun) )
import Data.Tree

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL l f) = GL (emp : l) (f + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0

instance Semigroup GuestList where
  (<>) (GL a b) (GL c d) = GL (a ++ c) (b + d) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b

-- Exercise 2

treeFold :: (b -> Tree a -> b) -> b -> Tree a -> b
treeFold f acc tree = foldl f (f acc tree) $ subForest tree