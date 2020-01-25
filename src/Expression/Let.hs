-- | A small language with let-bindings but without control flow or function calls.

module Expression.Let where

import Prelude
import Data.Maybe
import Data.Map (Map)

import qualified Data.Map as Map

data Expr = I Int | Op BinOp Expr Expr
    | Let String Expr Expr | Var String
    deriving (Show)

data BinOp = Add | Sub | Mul
    deriving (Show)

data Error = Undefined String

eval :: Map String Int -> Expr -> Either Error Int
eval _ (I i) = Right i
eval env (Op op a b) = f <$> eval env a <*> eval env b where
  f = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
eval env (Var v) = case Map.lookup v env of
    Just i -> Right i
    Nothing -> Left (Undefined v)
eval env (Let name rhs inExpr) = do
    value <- eval env rhs
    eval (Map.insert name value env) inExpr

-- TODO check that variables are defined / in scope before use; a vestigial type checker
