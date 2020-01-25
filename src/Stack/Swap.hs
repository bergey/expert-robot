-- | A stack language, with integers, suitable as a compilation target
-- or IR for expression languages.  Without jumps or RAM, it's not
-- powerful enough as a compilation target for a lambda calculus.
-- Swapping makes it equal in power to a register language.  A
-- 3-operand instruction in the register language is equivalent to a
-- secquence of Swaps & the corresponding 0-operand Op.

module Stack.Swap where

import Prelude
import Control.Arrow (first)
import Numeric.Natural

data Instruction = Push Int | Pop | Op BinOp | Swap Natural
    deriving (Show)

data BinOp = Add | Sub | Mul
    deriving (Show)

data Error = Underflow

eval :: [Int] -> [Instruction] -> Either Error [Int]
eval xs (Swap 0 : is) = eval xs is
eval xs (Swap n : is) = case split n xs of
    Just (a:as, b:bs) -> eval (b:as ++ a:bs) is
    Just ([], xs) -> eval xs is
    Just (_, []) -> Left Underflow
    Nothing -> Left Underflow
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

-- Unlike the version in Data.List, this gives a clear error if the
-- input list is not long enough.
split :: Natural -> [a] -> Maybe ([a], [a])
split 0 xs = Just ([], xs)
split _ [] = Nothing
split n (x : xs) = first (x:) <$> split (pred n) xs
