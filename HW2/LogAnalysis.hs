{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage x = case words x of
  ("I" : time : xs) -> LogMessage Info (read time) (unwords xs)
  ("E" : code : time : xs) -> LogMessage (Error $ read code) (read time) (unwords xs)
  ("W" : xs) -> LogMessage Info 0 (unwords xs)
  _ -> Unknown x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert log1@LogMessage {} Leaf = Node Leaf log1 Leaf
insert log1@(LogMessage _ t1 _) (Node left log2@(LogMessage _ t2 _) right)
  | t1 < t2 = Node (insert log1 left) log2 right
  | otherwise = Node left log2 (insert log1 right)
insert _ (Node _ (Unknown _) _) = Leaf -- Unknown logs shouldn't be in tree
insert (Unknown _) m = m

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter isImportantError . inOrder . build

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ s) = s
getMessage (Unknown _) = ""

isImportantError :: LogMessage -> Bool
isImportantError (LogMessage (Error code) _ _) = code >= 50
isImportantError _ = False