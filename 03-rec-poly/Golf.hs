module Golf where

import Data.List.Split

-- Main functions

skips :: [a] -> [[a]]
skips a = map (everyNth a) [1..length a]

-- Helper functions

everyNth :: [a] -> Int -> [a]
everyNth a n = concatMap (drop (n-1)) (chunksOf n a)