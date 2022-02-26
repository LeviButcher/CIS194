-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' x = product evens * product odds
  where
    evens = subtract 2 <$> filter even x
    odds = filter odd x

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . takeWhile (/= 0) . filter even . iterate choose
  where
    choose 1 = 0
    choose x
      | even x = x `div` 2
      | otherwise = 3 * x + 1

-- Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Not simple since it needs to be balanced. Don't think there is any way to easily get a balanced tree unless we
-- do balance correctness rotations

rootTree x = Node 0 Leaf x Leaf

foldTree :: [a] -> Tree a
foldTree = foldr addToTree Leaf
  where
    addToTree x Leaf = rootTree x
    addToTree x (Node h Leaf y r) = Node h (rootTree x) y r
    addToTree x (Node h l y Leaf) = Node h l y (rootTree x)
    addToTree x t@(Node h _ _ _) = Node (h + 1) t x Leaf

-- Exercise 3

xor :: [Bool] -> Bool
xor = odd . foldr ((+) . (\b -> if b then 1 else 0)) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x t -> f x : t) []

-- exercise 4
-- Skipping for now.