module JoinList where

-- import Data.Monoid

-- import Data.Monoid
import Sized (Sized (..), getSize)

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

-- jlTail :: JoinList m a -> Maybe (JoinList m a)

-- jlTail (Append)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Append m l r)
  | i < (getSize . tag $ l) = indexJ (pred 1) l
  | otherwise = indexJ (pred 1) r
indexJ 0 (Single _ a) = Just a
indexJ _ _ = Nothing

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r