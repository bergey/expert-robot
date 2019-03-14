-- | A language with Cons lists.

module Interpreters.Lists where

import Data.Foldable
import Data.List.NonEmpty

data Term = IT Int | ConsT Term Term | NilT
    deriving (Show, Eq)

data Expr = I Int | Op BinOp Expr Expr
    | Cons Expr Expr | Nil | Car Expr | Cdr Expr
    deriving (Show)

data BinOp = Add | Sub | Mul
    deriving Show

eval :: Expr -> Either String Term
eval (I i) = Right (IT i)
eval Nil = Right NilT
eval (Op op a b) = do
    a' <- eval a
    b' <- eval b
    f a' b'
  where
    f = case op of
      Add -> liftT (+)
      Sub -> liftT (-)
      Mul -> liftT (*)
    liftT :: (Int -> Int -> Int) -> Term -> Term -> Either String Term
    liftT f x y = case (x, y) of
        (IT x, IT y) -> Right . IT $ f x y
        _ -> Left "cannot do math on a list / Cons cell"
eval (Cons h t) = ConsT <$> eval h <*> eval t
eval (Car e) = do
    t <- eval e
    case t of
        IT _ -> Left "cannot take the Car of an integer"
        NilT -> Left "cannot take the Car of Nil"
        ConsT h _ -> Right h
eval (Cdr e) = do
    t <- eval e
    case t of
        IT _ -> Left "cannot take the Cdr of an integer"
        NilT -> Left "cannot take the Cdr of Nil"
        ConsT _ t -> Right t
