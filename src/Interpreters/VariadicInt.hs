-- | Have operators take arbitrary number of arguments, resolve at runtime.

module Interpreters.VariadicInt where

import Data.Foldable
import Data.List.NonEmpty

data Expr = I Int | Op Op (NonEmpty Expr)
    deriving (Show)

data Op = Add | Sub | Mul
    deriving Show

eval :: Expr -> Int
eval (I i) = i
eval (Op op (a :| as)) = foldl' f (eval a) (fmap eval as) where
  f = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
