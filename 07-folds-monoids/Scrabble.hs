{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Maybe
import qualified Data.Map.Strict as Map

newtype Score = Score Int
  deriving (Eq, Read, Show, Ord, Num)

instance Monoid Score where
  mempty = Score 0

instance Semigroup Score where
  Score a <> Score b = Score (a + b)

scores :: Map.Map Char Score
scores = Map.fromList [
    ('a', Score 1),
    ('b', Score 3),
    ('c', Score 3),
    ('d', Score 2),
    ('e', Score 1),
    ('f', Score 4),
    ('g', Score 2),
    ('h', Score 4),
    ('i', Score 1),
    ('j', Score 8),
    ('k', Score 5),
    ('l', Score 1),
    ('m', Score 3),
    ('n', Score 1),
    ('o', Score 1),
    ('p', Score 3),
    ('q', Score 10),
    ('r', Score 1),
    ('s', Score 1),
    ('t', Score 1),
    ('u', Score 1),
    ('v', Score 4),
    ('w', Score 4),
    ('x', Score 8),
    ('y', Score 4),
    ('z', Score 10)
  ]

score :: Char -> Score
score = fromMaybe 0 . flip Map.lookup scores

scoreString :: String -> Score
scoreString = foldl (<>) (Score 0) . map score