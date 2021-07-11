import Sized

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