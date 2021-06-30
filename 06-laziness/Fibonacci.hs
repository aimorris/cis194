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
  show (Cons a x) = show a ++ " : " ++ show x

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a x) = Cons (f a) (streamMap f x)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f $ f a