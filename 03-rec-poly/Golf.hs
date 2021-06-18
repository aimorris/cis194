module Golf where

import Data.List.Split

-- Main functions

skips :: [a] -> [[a]]
skips a = map (everyNth a) [1..length a]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) =
  if x < y && y > z then y : localMaxima (y:z:zs) else localMaxima (y:z:zs)
localMaxima _ = []

-- Helper functions

everyNth :: [a] -> Int -> [a]
everyNth a n = concatMap (drop (n-1)) (chunksOf n a)

histogram :: [Integer] -> String
histogram x = unlines (map (oneLine (count x)) [9,8..1]) ++ "==========\n0123456789\n"

oneLine :: [Int] -> Int -> String
oneLine x y = map (\n -> if n >= y then '*' else ' ') x

count :: [Integer] -> [Int]
count x = map (\n -> length (filter (== n) x)) [0..9]