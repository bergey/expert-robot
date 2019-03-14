{-# LANGUAGE ExplicitForAll #-}
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

eval :: Expr -> Term
eval (I i) = IT i
eval (D d) = DT d
eval (Op op a b) = f (eval a) (eval b) where
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

testCase :: String -> Bool -> Maybe String
testCase err cond = if cond then Nothing else Just err

tests :: [String] -- failures
tests = catMaybes
    [ testCase "int" (eval (I 123) == IT 123)
    , testCase "double" (eval (D 1.23) == DT 1.23)
    , testCase "add" (eval (Op Add (I 1) (I 2)) == IT 3)
    ]
