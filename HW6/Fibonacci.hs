import Control.Monad (join)
import Data.List (findIndex, unfoldr)
import Data.Maybe (fromJust)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0 ..]

fibs2 :: [Integer]
fibs2 = unfoldr (\(x, y) -> Just (x, (y, x + y))) (0, 1)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = unwords . map show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

powerOf2 :: [Integer]
powerOf2 = iterate (2 *) 1

powersOf2S :: Stream Integer
powersOf2S = streamFromSeed (2 *) 1

largestPowerOf2 :: Integer -> Integer
largestPowerOf2 x
  | 0 == x = 0
  | odd x = 0
  | otherwise = maximum . takeWhile (/= 0) $ map (x `div`) (iterate (2 *) 1)

-- 6/2^0 = 6
-- 6/2^1 = 3
-- 6/2^2 = 6/4 xxx
-- 6/2^3 = 6/8 xx

-- toInteger . fromJust $

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x s) (Cons y s2) = Cons x (Cons y (interleaveStream s s2))

evens :: Stream Integer
evens = streamFromSeed (+ 2) 2

ruler :: Stream Integer
ruler = interleaveStream (streamRepeat 0) ruled
  where
    ruled = streamMap largestPowerOf2 evens