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