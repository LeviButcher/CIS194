module Golf where

import Control.Applicative (ZipList (..))
import Control.Monad.RWS (MonadReader (local), join)
import Data.List (transpose)
import Debug.Trace (trace)

everyNth :: Int -> [a] -> [a]
everyNth n x = everyNth' n $ zip [1 ..] x
  where
    everyNth' :: Int -> [(Int, a)] -> [a]
    everyNth' n [] = []
    everyNth' n ((i, x) : xs)
      | i `mod` n == 0 = x : everyNth' n xs
      | otherwise = everyNth' n xs

-- I think this could be better with applicative?
skips :: [a] -> [[a]]
skips x = take (length x) $(\f -> f x) <$> (everyNth <$> [1 ..])

localMaxima :: [Integer] -> [Integer]
localMaxima x@(a : b : c : _)
  | a < b && b > c = b : localMaxima (tail x)
  | otherwise = localMaxima (tail x)
localMaxima _ = []

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

fillToLength :: Int -> a -> [a] -> [a]
fillToLength n d x = x ++ replicate (n - length x) d

-- ["  ", "**", "* "]
-- [" * "]
-- [" **"]

zipConcat :: [String] -> [String]
zipConcat (a : b : xs) = zipWith (++) (zipWith (\a b -> a : [b]) a b) (zipConcat xs)
zipConcat [a] = [a]
zipConcat [] = []

-- TODO: Need to figure out how to correctly join h to be the histogram string

histogram :: [Integer] -> String
histogram x =
  let numbs = [0 .. 9]
      h = (\x -> if null x then " " else x) . flip replicate '*' . (\f -> f x) . count <$> numbs
      longest = maximum . map length $ h
      hStrings = unlines . zipConcat $ fillToLength longest ' ' <$> h
      temp = trace (show hStrings) 5
      bar = replicate (length numbs) '='
      numbString = "0123456789"
   in unlines [hStrings, bar, numbString]