-- Main

fun1' :: [Integer] -> Integer
fun1' =
  product .
  map (subtract 2) .
  filter even

fun2' :: Integer -> Integer
fun2' =
  sum .
  filter even .
  takeWhile (/= 1) .
  iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- Helper

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node h l n r)
  | leftHeight > rightHeight = Node h l n (insertTree x r)
  | rightHeight > leftHeight = Node h (insertTree x l) n r
  | otherwise = Node (max leftHeight (getTreeHeight (insertTree x r)) + 1) l n (insertTree x r)
  where leftHeight = getTreeHeight l
        rightHeight = getTreeHeight r

getTreeHeight :: Tree a -> Integer
getTreeHeight Leaf = 0
getTreeHeight (Node h _ _ _) = h

-- Supplied

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)