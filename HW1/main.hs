import Control.Applicative
import Control.Monad (join)

-- Credit Card Validation Exercise

charToInt :: Char -> Integer
charToInt = read . pure

toDigits :: Integer -> [Integer]
toDigits = map charToInt . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x
  | even (length x) = getZipList $ ZipList (cycle [(* 2), (* 1)]) <*> ZipList x
  | otherwise = getZipList $ ZipList (cycle [(* 1), (* 2)]) <*> ZipList x

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . join . map toDigits . doubleEveryOther . toDigits

-- Tower of Hanoi Exercise

type Peg = String

type Move = (Peg, Peg)

tempMove a b temp = [(a, temp), (a, b), (temp, b)]
