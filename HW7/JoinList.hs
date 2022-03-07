{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Buffer
import Data.Monoid
import Editor (editor, runEditor)
import Scrabble
import Sized (Size (Size), Sized (..), getSize)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
-- tag Empty =  Empty
tag (Single m a) = m
tag (Append m _ _) = m
tag Empty = mempty

jlHead :: JoinList m a -> Maybe a
jlHead Empty = Nothing
jlHead (Single _ a) = Just a
jlHead (Append _ l _) = jlHead l

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Append m l r)
  | i < sizeL = indexJ i l
  | otherwise = indexJ (i - sizeL) r
  where
    sizeL = getSize . size . tag $ l
indexJ 0 (Single _ a) = Just a
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ n Empty = Empty
dropJ n (Single m a) = Empty
dropJ n (Append m l r)
  | n >= sizeT = Empty
  | n == sizeL = Empty +++ r
  | n == sizeR = l +++ Empty
  | n < sizeL = dropJ n l +++ r
  | otherwise = l +++ dropJ n r
  where
    sizeL = getSize . size . tag $ l
    sizeR = getSize . size . tag $ l
    sizeT = getSize . size $ m

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n (Single m a)
  | n == 0 = Empty
  | otherwise = Single m a
takeJ n (Append m l r)
  | n < sizeL = takeJ n l
  | n == sizeL = l
  | n < sizeT = l +++ takeJ (n - sizeL) r
  | otherwise = Append m l r
  where
    sizeL = getSize . size . tag $ l
    sizeR = getSize . size . tag $ l
    sizeT = getSize . size $ m

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

testList = Single (Size 1) '0' +++ Single (Size 1) '1' +++ Single (Size 1) '2'

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldl (+++) Empty . map (\x -> Single (scoreString x, 1) x) . lines
  line = indexJ
  replaceLine = undefined
  numLines = getSize . snd . tag
  value = fst . tag

main :: IO ()
main = do
  let b =
        fromString . unlines $
          [ "This buffer is for notes you don't want to save, and for",
            "evaluation of steam valve coefficients.",
            "To load a different file, type the character L followed",
            "by the name of the file."
          ] ::
          JoinList (Score, Size) String
  runEditor editor b
