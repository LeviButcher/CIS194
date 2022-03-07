module Party where

import Data.Tree (Tree (Node))
import Employee (Employee (Emp), GuestList (GL))

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) gl = GL [e] f <> gl

instance Semigroup GuestList where
  (GL l lf) <> (GL r rf) = GL (l ++ r) (lf + rf)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l r
  | l > r = l
  | otherwise = r

treeFold :: ([b] -> a -> b) -> b -> Tree a -> b
treeFold f s (Node x []) = f [s] x
treeFold f s (Node x l) = f (map (treeFold f s) l) x

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = undefined