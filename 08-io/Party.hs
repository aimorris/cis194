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

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f acc tree = f (rootLabel tree) $ map (treeFold f acc) (subForest tree)

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  where withoutBoss = maximum $ map fst gls
        withBoss = maximum $ map (glCons boss . snd) gls

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel (mempty, mempty)