{-# OPTIONS_GHC -Wall #-}

-- Credit Card

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
  | n < 0 = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:z) = x : y*2 : doubleEveryOtherRev z

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

-- Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 0 _ _ _ _ = []
hanoiFour n a b c d =
  let k = n - round(sqrt(2*(fromIntegral n :: Double) + 1)) + 1
  in hanoiFour k a c d b ++ hanoi (n - k) a b d ++ hanoiFour k c b d a
