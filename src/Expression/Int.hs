-- | A very simple expression language

module Expression.Int where

import Prelude
import Data.Maybe

data Expr = I Int | Op BinOp Expr Expr
    deriving (Show)

data BinOp = Add | Sub | Mul
    deriving (Show)

eval :: Expr -> Int
eval (I i) = i
eval (Op op a b) = f (eval a) (eval b) where
  f = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)

testCase :: String -> Bool -> Maybe String
testCase err cond = if cond then Nothing else Just err

tests :: [String] -- failures
tests = catMaybes
    [ testCase "literal" (eval (I 123) == 123)
    , testCase "add" (eval (Op Add (I 1) (I 2)) == 3)
    ]
