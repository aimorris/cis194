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
streamToList (Cons a x) = a : streamToList x

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a x) = Cons (f a) (streamMap f x)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f $ f a

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

largestBase2Exp :: Integer -> Integer -> Integer -> Integer
largestBase2Exp lar exp n
  | n < 2^exp = lar
  | n `mod` 2^exp == 0 = largestBase2Exp exp (exp + 1) n
  | otherwise = largestBase2Exp lar (exp + 1) n

ruler :: Stream Integer
ruler = streamMap (largestBase2Exp 0 0) nats

-- Exercise 4 (no divisibility checking)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a $ Cons b $ interleaveStreams as bs

-- Doesn't display
ruler' :: Stream Integer
ruler' = foldr1 interleaveStreams $ map streamRepeat [0..]