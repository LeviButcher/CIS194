{-# LANGUAGE TypeSynonymInstances #-}

module Scrabble where

import Data.Char (toUpper)

type Score = Int

-- PAIN
score :: Char -> Score
score = go . toUpper
  where
    go 'A' = 1
    go 'B' = 3
    go 'C' = 3
    go 'D' = 2
    go 'E' = 1
    go 'F' = 4
    go 'G' = 2
    go 'H' = 4
    go 'I' = 1
    go 'J' = 8
    go 'K' = 5
    go 'L' = 1
    go 'M' = 3
    go 'N' = 1
    go 'O' = 1
    go 'P' = 3
    go 'Q' = 10
    go 'R' = 1
    go 'S' = 1
    go 'T' = 1
    go 'U' = 1
    go 'V' = 4
    go 'W' = 4
    go 'X' = 8
    go 'Y' = 4
    go 'Z' = 10
    go _ = 0

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mappend = (+)
  mempty = 0

scoreString :: String -> Score
scoreString = mconcat . map score