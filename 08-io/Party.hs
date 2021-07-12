{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee ( GuestList(..), Employee(empFun) )

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL l f) = GL (emp : l) (f + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0

instance Semigroup GuestList where
  (<>) (GL a b) (GL c d) = GL (a ++ c) (b + d) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b