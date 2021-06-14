{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)