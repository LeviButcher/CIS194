{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative (liftA2)
import ExprT (ExprT (..))
import Parser (parseExp)
import StackVM (Program, StackExp (PushI))
import qualified StackVM as StackExp

eval :: ExprT -> Integer
eval (Mul l r) = eval l * eval r
eval (Add l r) = eval l + eval r
eval (Lit x) = x

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

class Expr a where
  lit :: Integer -> a

  add :: a -> a -> a

  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add l r = Add l r
  mul l r = Mul l r

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

reify :: ExprT -> ExprT
reify = id

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq)

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

mod7 :: Integer -> Integer
mod7 = (`mod` 7)

instance Expr Mod7 where
  lit x = Mod7 $ mod7 x

  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr Program where
  lit x = [PushI x]

  add l r = l ++ r ++ [StackExp.Add]

  mul l r = l ++ r ++ [StackExp.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
