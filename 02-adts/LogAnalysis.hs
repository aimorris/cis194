{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage string =
  case words string of
    ("I":y:ys) -> LogMessage Info (read y) (unwords ys)
    ("W":y:ys) -> LogMessage Warning (read y) (unwords ys)
    ("E":y:z:zs) -> LogMessage (Error (read y)) (read z) (unwords zs)
    x -> Unknown (unwords x)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert
  x@(LogMessage _ insertTime _)
  (Node leftTree middle@(LogMessage _ middleTime _) rightTree) =
    if insertTime > middleTime
    then Node leftTree middle (insert x rightTree)
    else Node (insert x leftTree) middle rightTree
insert x Leaf = Node Leaf x Leaf
insert _ y = y

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left middle right) = inOrder left ++ [middle] ++ inOrder right