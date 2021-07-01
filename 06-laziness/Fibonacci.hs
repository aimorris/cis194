{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : scanl (+) 1 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat y = Cons y $ streamRepeat y

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f $ f a

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

largestBase2Exp :: Integer -> Integer -> Integer -> Integer
largestBase2Exp lar expon n
  | n < 2^expon = lar
  | n `mod` 2^expon == 0 = largestBase2Exp expon (expon + 1) n
  | otherwise = largestBase2Exp lar (expon + 1) n

ruler :: Stream Integer
ruler = streamMap (largestBase2Exp 0 0) nats

-- Exercise 5 (no divisibility checking)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a $ Cons b $ interleaveStreams as bs

-- Doesn't display
ruler' :: Stream Integer
ruler' = foldr1 interleaveStreams $ map streamRepeat [0..]

-- Exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  (+) (Cons a as) (Cons b bs) = Cons (a + b) $ as + bs
  (*) (Cons a as) bf@(Cons b bs) = Cons (a * b) $ streamMap (* a) bs + as * bf
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons a as) = Cons (negate a) $ negate as

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = q
    where q = Cons (a `div` b) $ streamMap (`div` b) $ as - q * bs

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)