import Control.Monad (join)

charToInt :: Char -> Integer
charToInt = read . pure

toDigits :: Integer -> [Integer]
toDigits = (map charToInt) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherInOrder . reverse

doubleEveryOtherInOrder :: [Integer] -> [Integer]
doubleEveryOtherInOrder (x:y:xs) = x:y*2:doubleEveryOtherInOrder xs
doubleEveryOtherInOrder (x:[]) = [x]
doubleEveryOtherInOrder [] = []

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . join . (map toDigits) . doubleEveryOther . toDigits 