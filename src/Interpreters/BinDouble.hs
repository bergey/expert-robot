{-# LANGUAGE RankNTypes #-}
-- | An expression language with two primitive types, and hence with
-- runtime errors.

module Interpreters.BinDouble where

import Prelude
import Data.Maybe

data Term = IT Int | DT Double
    deriving (Show, Eq)

data Expr = I Int | D Double | Op BinOp Expr Expr
    deriving (Show)

data BinOp = Add | Sub | Mul | Div
    deriving (Show)

-- | This evaluator promotes integers to floating point
evalPromoting :: Expr -> Term
evalPromoting (I i) = IT i
evalPromoting (D d) = DT d
evalPromoting (Op op a b) = f (evalPromoting a) (evalPromoting b) where
    liftT :: (forall a. Num a => a -> a -> a) -> Term -> Term -> Term
    liftT f x y = case (x, y) of
      (IT i, IT j) -> IT $ f i j
      (DT x, DT y) -> DT $ f x y
      (IT i, DT x) -> DT $ f (fromIntegral i) x
      (DT x, IT i) -> DT $ f x (fromIntegral i)
    f :: Term -> Term -> Term
    f = case op of
        Add -> liftT (+)
        Sub -> liftT (-)
        Mul -> liftT (*)
        Div -> \x y -> DT $ case (x,y) of
            (DT x, DT y) -> x / y
            (IT i, DT x) -> fromIntegral i / x
            (DT x, IT i) -> x / fromIntegral i
            (IT i, IT j) -> fromIntegral i / fromIntegral j

data Error = TypeMismatch Term Term
    | IntDivision Term Term
    deriving Show

eval :: Expr -> Either Error Term
eval (I i) = Right $ IT i
eval (D d) = Right $ DT d
eval (Op op a b) = do
    a' <- eval a
    b' <- eval b
    f a' b'
  where
    liftT :: (forall a. Num a => a -> a -> a) -> Term -> Term -> Either Error Term
    liftT f x y = case (x, y) of
      (IT i, IT j) -> Right (IT (f i j))
      (DT x, DT y) -> Right (DT (f x y))
      _ -> Left (TypeMismatch x y)
    f :: Term -> Term -> Either Error Term
    f = case op of
        Add -> liftT (+)
        Sub -> liftT (-)
        Mul -> liftT (*)
        Div -> \x y -> case (x,y) of
            (DT x, DT y) -> Right (DT (x / y))
            _ -> Left (IntDivision x y)

testCase :: String -> Bool -> Maybe String
testCase err cond = if cond then Nothing else Just err

tests :: [String] -- failures
tests = catMaybes
    [ testCase "int" (evalPromoting (I 123) == IT 123)
    , testCase "double" (evalPromoting (D 1.23) == DT 1.23)
    , testCase "add" (evalPromoting (Op Add (I 1) (I 2)) == IT 3)
    ]
