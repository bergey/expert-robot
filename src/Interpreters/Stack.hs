-- | A stack language, with integers, suitable as a compilation target
-- or IR for expression languages.  Without jumps or RAM, it's not
-- powerful enough as a compilation target for a lambda calculus.

module Interpreters.Stack where

import Prelude

data Instruction = Push Int | Pop | Op BinOp
    deriving (Show)

data BinOp = Add | Sub | Mul
    deriving (Show)

data Error = Underflow

eval :: [Int] -> [Instruction] -> Either Error [Int]
eval xs (Push x : is) = eval (x : xs) is
eval (_ : xs) (Pop : is) = eval xs is
eval [] (Pop : _) = Left Underflow
eval (x : y : xs) (Op op : is) = eval (f x y : xs) is where
  f = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
eval _ (Op _ : _) = Left Underflow
eval xs [] = return xs
