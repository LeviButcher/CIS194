module Party where

import Data.List (sort)
import Data.Tree (Tree (Node))
import Employee (Employee (Emp, empName), GuestList (GL))

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) gl = GL [e] f <> gl

instance Semigroup GuestList where
  (GL l lf) <> (GL r rf) = GL (l ++ r) (lf + rf)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l r = if l > r then l else r

treeFold :: ([b] -> a -> b) -> b -> Tree a -> b
treeFold f s (Node x []) = f [s] x
treeFold f s (Node x l) = f (map (treeFold f s) l) x

-- Not sure if all the way correct
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gl = (glCons e largestWithBoss, largestNonBoss)
  where
    largestWithBoss = maximum . map fst $ gl
    largestNonBoss = maximum . map snd $ gl

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold (flip nextLevel) (mempty, mempty)

main :: IO ()
main = do
  empTree <- read <$> readFile "company.txt" :: IO (Tree Employee)
  let (GL guestList fun) = maxFun empTree
  putStrLn $ "Total Fun: " ++ show fun
  let formattedList = unlines . sort . map empName $ guestList
  putStrLn formattedList

  return ()