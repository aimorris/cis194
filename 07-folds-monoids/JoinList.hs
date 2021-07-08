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
indexJ i (Single m a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append m a b)
  | i < 0 = Nothing
  | otherwise = if i > getSize (size m) then indexJ (i - 1) b else indexJ (i - 1) a