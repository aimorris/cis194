import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append m a b)
  | i < 0 = Nothing
  | i < lengthA = indexJ i a
  | otherwise = indexJ (i - lengthA) b
  where lengthA = getSize (size $ tag a)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n a | n <= 0 = a
dropJ _ Empty = Empty
dropJ n (Single _ _) = Empty
dropJ n jl@(Append m a b)
  | n == lengthA = b
  | n < lengthA = Append m (dropJ n a) b
  | otherwise = dropJ (n - lengthA) b
  where lengthA = getSize (size $ tag a)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ n jl@(Single _ _) = jl
takeJ n jl@(Append m a b)
  | n == lengthA = a
  | n < lengthA = takeJ n a
  | otherwise = Append m a (takeJ (n - lengthA) b)
  where lengthA = getSize (size $ tag a)

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine = Single =<< scoreString

-- Helper

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

test1 :: JoinList Size [Char]
test1 = Append (Size 3)
      (Append (Size 2)
        (Single (Size 1) "first")
        (Single (Size 1) "second")
      )
     (Single (Size 1) "third")

test2 :: JoinList Size [Char]
test2 = Single (Size 1) "first"

test3 :: JoinList Size [Char]
test3 = Append (Size 2)
      (Single (Size 1) "first")
      (Single (Size 1) "second")