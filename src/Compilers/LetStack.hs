-- | Compile let-bindings to a stack.

module Compilers.LetStack where

import qualified Expression.Let as E
import qualified Stack.RandomAccess as S

import Data.List (elemIndex)
import Data.Map (Map)
import Data.Word (Word)

import qualified Data.Map as Map

compile :: E.Expr -> Either E.Error [S.Instruction]
compile = fmap reverse . compile' []

-- a simulation of the stack, tracking current depth of each named value
type Stack = [Maybe String]

compile' :: Stack -> E.Expr -> Either E.Error [S.Instruction]
compile' names (E.Var n) = case elemIndex (Just n) names of
    Nothing -> Left (E.Undefined n)
    Just i -> Right [S.Load (fromIntegral i)]
compile' _ (E.I i) = Right [S.Push i]
compile' names (E.Op op a b) = do
    a' <- compile' names a
    b' <- compile' (Nothing : names) b
    let op' = case op of
            E.Add -> S.Add
            E.Sub -> S.Sub
            E.Mul -> S.Mul
    return (S.Op op' : b' ++ a')
compile' names (E.Let n rhs inExpr) = do
    rhs1 <- compile' names rhs
    inExpr1 <- compile' (Just n : names) inExpr
    return (S.Delete 1 : inExpr1 ++ rhs1)

-- | Probably it's a bit faster if we use a Map
compileMap :: Map String Word -> E.Expr -> Either E.Error [S.Instruction]
compileMap names (E.Var n) = case Map.lookup n names of
    Nothing -> Left (E.Undefined n)
    Just i -> Right [S.Load (fromIntegral i)]
compileMap _ (E.I i) = Right [S.Push i]
compileMap names (E.Op op a b) = do
    a' <- compileMap names a
    b' <- compileMap (fmap (+1) names) b
    let op' = case op of
            E.Add -> S.Add
            E.Sub -> S.Sub
            E.Mul -> S.Mul
    return (S.Op op' : b' ++ a')
compileMap names (E.Let n rhs inExpr) = do
    rhs1 <- compileMap names rhs
    inExpr1 <- compileMap (Map.insert n 0 names) inExpr
    return (S.Delete 1 : inExpr1 ++ rhs1)
