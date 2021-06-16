{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

test :: [LogMessage]
test = [LogMessage Info 6 "Completed armadillo processing",LogMessage Info 1 "Nothing to report",LogMessage Info 4 "Everything normal",LogMessage Info 11 "Initiating self-destruct sequence",LogMessage (Error 70) 3 "Way too many pickles",LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",LogMessage Warning 5 "Flange is due for a check-up",LogMessage Info 7 "Out for lunch, back in two time steps",LogMessage (Error 20) 2 "Too many pickles",LogMessage Info 9 "Back from lunch",LogMessage (Error 99) 10 "Flange failed!"]

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

filterError :: LogMessage -> Bool
filterError (LogMessage (Error sev) _ _)
  | sev > 50 = True
  | otherwise = False
filterError _ = False

extractString :: LogMessage -> String
extractString (LogMessage _ _ msg) = msg
extractString (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map extractString $ filter filterError $ inOrder $ build x